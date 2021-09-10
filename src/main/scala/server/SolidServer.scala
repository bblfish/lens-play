package server

import monocle.function.{At, Index}
import monocle.syntax.all.*
import monocle.{Focus, Iso, Lens, Optional, PLens}

import java.time.{Clock, Instant}
import scala.collection.immutable.*

/**
 * The aim of this is to see what a very primitive web server using Lenses
 * would look like, to see if this can bring some extra conceptual clarity
 * to questions in Solid.
 * See [[https://gitlab.com/web-cats/CG/-/issues/28 Web Cats Lenses issue 28]].
 * For a nice historical overview of Lenses see Jules Hedges 2018 blog post [[https://julesh.com/2018/08/16/lenses-for-philosophers/ Lenses for Philosophers]]
 *
 * Thanks to Ken Scambler for major simplifications using Enum and list based indexes
 * [[https://discord.com/channels/632277896739946517/882978685936877608/883198894350163989 in Typelevel Monocle Discord Channel]]
 * (That was evident in the previous version at Server.scala, and is a bit lost here)

 * For an intro to Monocle see the video https://twitter.com/bblfish/status/1413758476896153601
 * and the thread of references above it. Documentation https://www.optics.dev/Monocle/
 *
 *  This (server.Solid) is an improvement over server.Server.scala in the following way: <ol>
 *  <li> We use Lenses to select paths, as this gives CRUD (create, read, update, delete) functionality
 *  <li> To use lenses and paths, we need paths to always have a reference in a
 *  Container whatever their length, even if their value is just empty.
 *  As a consequence creating a resource at a path, automatically creates all intermediate
 *  containers.
 *  <li> If we are going to be able to write to paths at any depth, we need to not overwrite an
 *    existing resource with a Container by accident. We don't want to be testing for each
 *    path if it is referring to a non-container or a container. So we distinguish them
 *    syntactically: paths ending in `/` refer to containers and paths without to non
 *    container resources. This allow us to have <ul>
 *     <li> /foo <- resource
 *     <li> /foo/ <- container
 *     <li> /foo/bar <- resource
 *     <li> /foo/bar/ <-container
 *     <li>  ...
 *    </ul>
 *  </ol>
 *  Having a direct mapping such as this is what I called an Intutive Container
 *  https://www.w3.org/2012/ldp/track/issues/50
 *  And it is what Solid recommends, but does not provide a way to describe (yet).
 *  Hence we call this here a [[https://solidproject.org/TR/ Solid Server]].
 */
object SolidServer:
	/** segments are strings that cannot contain `/`
	eg. here we have 2 segments and 1 file `/segment1/segment2/file`
	*/
	case class Segment(name: String)

	/** A Path is
	 *   <li> either a container path (CPath),
	 *   e.g. `/` or `/foo/bar/` or
	 *   <li> or a Path for a file
	 *   e.g. `/readme.txt` or `/people/henry/card`
	*/
	enum Path:
		case CPath(path: List[Segment])
		case FPath(name: String, cpath: CPath)

	/** LDP stands for [[https://www.w3.org/TR/ldp/ Linked Data Platform]]
	 *  This is a simplified model.
	 *
	 *  Resources have content and metadata, which we illustrate with `created` field.
	 *  Resource is a recursive datatype that allows us to model a nested Web Server
	 *  resource hierarchy.
	 *
	 * Locating a resource is just a matter of following the Map hierarchy deeper
	 * and deeper inwards.
	 * */
	enum Solid:
		//we allow a container to have a resource and a container with the same name
		//This allows one to create containers without overwriting old resources
		//it is equivalent to allowing `/foo` and `/foo/`
		//With a Heterogenoous Map one could combine those two maps into one, and have the type of the map value
		//depend on the type of the key.
		case Container(
			subContainers: Map[Segment,Container] = Map[Segment,Container](),
			nonCR: Map[String,NonCR] = Map[String,NonCR](),
			created: Instant = now
		)

		/* Non Container Resource (example) one could cover all with a
		* binary content and mime types.*/
		case NonCR(content: String, created: Instant = now)

		def summary: String = this match
			case Container(content, files, _) => (content.keys.map(_.name + "/") ++ files.keys).mkString("- ", "\n- ", "\n")
			case NonCR(content, _) => content

	end Solid

	def now: Instant = Clock.systemUTC().instant()
	val slugCounter: Iterator[Int] = new Iterator[Int] {
		var c = 0
		override def hasNext: Boolean = true
		override def next(): Int = c + 1
	}

	/** We can think of a web server as just a root container with a number of resources
	 * (which can themselves be containers), each named by a key of type String.
	 * A path of such strings gives a URL Path. */
	val root: Solid.Container = Solid.Container()

	object LDPC

	case class Response(code: Int, content: String)

	import Solid.*

	given At[Container, String, Option[NonCR]] =
		At(key => Focus[Container](_.nonCR.at(key)))

	given At[Container, Segment, Option[Container]] =
		At(key => Focus[Container](_.subContainers.at(key)))

	// it should be possible to compose this recursively,
	// but I need to create an At[Option[Container], Path.CPath, Option[Container]]
	// from the given above
	given At[Container, Path.CPath, Option[Container]] =
		At[Container, Path.CPath, Option[Container]]( cp =>
			Lens[Container,Option[Container]]( (container: Container) =>
				def find(c: Container, remaining: List[Segment]): Option[Container] =
					remaining match
					case Nil => Some(c)
					case head::tail => c.subContainers.get(head).flatMap(cc => find(cc,tail))
				find(container,cp.path)
			){replaceCntrOpt => rootContainer =>
				def interm(cntr: Container, remaining: List[Segment]): Option[Container] =
					remaining match
					case Nil =>  replaceCntrOpt
					case head::tail =>
						val newMap: Map[Segment, Container] = cntr.subContainers.updatedWith(head){
							case Some(subCntr) => interm(subCntr,tail)
							case None => interm(Container(),tail)
						}
						Some(cntr.copy(subContainers = newMap))

				interm(rootContainer,cp.path).getOrElse(rootContainer)
			}
		)

	given (
		// how do I lift fileAt to At[Option[Container]... ?
		using fileAt: At[Container, String, Option[NonCR]],
		crAt: At[Container, Path.CPath, Option[Container]]
	): At[Container, Path.FPath, Option[NonCR]] =
		At[Container, Path.FPath, Option[NonCR]]{ (path: Path.FPath) =>
			crAt.at(path.cpath) andThen
				Lens[Option[Container], Option[NonCR]](_.flatMap(_.nonCR.get(path.name)))(
					newResOpt => _.map { cntr =>
							val nonCrMap = cntr.nonCR.updatedWith(path.name)(_ => newResOpt)
							cntr.copy(nonCR = nonCrMap)
						}
				)
		}

	val rt = Iso.id[Container]

	/** typeclass for Index on a Container type S given a List of indexes.
	 * This is defined recursively.
	 * thanks to Ken Scambler */
//	given [S, I](using Index[S, I, S]): Index[S, List[I], S] =
//	Index {
//		case Nil => Focus[S]()
//		case key :: keys => Focus[S](_.index(key).index(keys))
//	}

	/** The HTTP methods that one can use to interact with a Container.
	 * See SolidServer.sc in this package for user.
	 * todo: write out <src/test/scala/server/SolidServerTests.scala> for usage.
	 * Docs to look at:
	 * [[https://www.optics.dev/Monocle/docs/faq FAQ on the difference between `index`` and `at`]]
	 * Notice that we have a lot of duplication, as we distinguish between contents that
	 * are containers and those that are "files". Would a Heterogenous map gotten by merging
	 * the subcontainers and the nonCR maps help?
	 * Would that require dependent lenses?
	 * */
	extension (server: Solid.Container)(using
		ctrAt: At[Solid.Container, Path.CPath, Option[Container]],
		fAt : At[Container, Path.FPath, Option[NonCR]]
	)
		def GET(path: Path): Response =
			path match
			case f: Path.FPath => rt.at(f).get(server)
			case c: Path.CPath => rt.at(c).get(server)
			match
				case Some(res: Solid) => Response(200, res.summary)
				case None => Response(404, "Content could not be found")

		def PUT(path: Path.FPath, nonCR: NonCR): Solid.Container =
			rt.at(path).replace(Some(nonCR))(server)

		def PUT(path: Path.CPath): Option[Solid.Container] =
			rt.at(path).replaceOption(Option(Container()))(server)

		def POST(path: Path.CPath)(
			slug: String, newcontent: String | LDPC.type
		): (Solid.Container, Response) =
			val ct: Option[Container] = rt.at[Path.CPath,Option[Container]](path).modifyOption { (containerOpt: Option[Container]) =>
				containerOpt.map { container =>
					newcontent match
						case ldpc: LDPC.type =>
							val name = if container.subContainers.get(Segment(slug)).isEmpty then slug
							else s"${slug}_${slugCounter.next()}"
							rt.at(Segment(name)).replace(Some(Container()))(container)
						case other: String =>
							val name = if container.nonCR.get(slug).isEmpty then slug
							else s"${slug}_${slugCounter.next()}"
							rt.at(name).replace(Some(NonCR(other)))(container)
				}
			}(server)
			ct match
				case Some(newC) => (newC, Response(200, "how do we pass the name of the new resource here?"))
				case None => (server, Response(404, "container does not exist"))

		def DELETE(path: Path): Solid.Container =
			path match
			case cp: Path.CPath => rt.at(cp).replace(None)(server)
			case fp: Path.FPath => rt.at(fp).replace(None)(server)
		//the duplication above could I guess be removed  with Heterogeneous Map for content & Dependently typed lenses

end SolidServer



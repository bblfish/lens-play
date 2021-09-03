package server

import cats.data.NonEmptyList as NEL
import monocle.{Focus, Lens, Optional}
import monocle.function.Index
import monocle.syntax.all.*

import java.time.{Clock, Instant}
import scala.collection.immutable.*

/**
 * The aim of this is to see what a very primitive web server using Lenses
 * would look like, to see if this can bring some extra conceptual clarity
 * to questions in Solid.
 * See [[https://gitlab.com/web-cats/CG/-/issues/28 Web Cats Lenses issue 28]].
 *
 * Thanks to Ken Scrambler for major simplifications using Enum and list based indexes
 * [[https://discord.com/channels/632277896739946517/882978685936877608/883198894350163989 in Typelevel Monocle Discord Channel]]
 */
object Server:
	//for an intro to Monocle see the video https://twitter.com/bblfish/status/1413758476896153601
	// and the thread of references above it.
	// documentation https://www.optics.dev/Monocle/

	// counter for Slugs
	val counter: Iterator[Int] = new Iterator[Int] {
		var c = 0
		override def hasNext: Boolean = true
		override def next(): Int = c+1
	}
	def now: Instant = Clock.systemUTC().instant()

	/** Resources have content and metadata, which we illustrate with `created` field.
	 * Resource is a recursive datatype that allows us to model a nested Web Server
	 * resource hierarchy.
    *
	 * Locating a resource is just a matter of following the Map hierarchy deeper
	 * and deeper inwards.
	 **/
	enum Resource:
		case Container(content: Map[String, Resource] = Map(), created: Instant = now)
		//we currently only have a TextResource, but we could add a mime type and have
		//the content as a binary string to cover all cases
		case TextResource(content: String, created: Instant = now)

		def summary: String = this match
			case Container(content, _) => content.keys.mkString("- ","\n- ","\n")
			case TextResource(content, _) => content

	/**
	 * We can think of a web server as just a root container with a number of resources
	 * (which can themselves be containers), each named by a key of type String.
	 * A path of such strings gives a URL Path.
	 */
	val root = Resource.Container()

	object LDPC
	case class Response(code: Int, content: String)

//	val contents = Lens[Container, Map[String, Resource[_]]](_.content){ newmap =>
//		cont => cont.copy(content = newmap)
//	}

	type Path = List[String]

	/**
	 * typeclass for a basic Index on a Container, locates it's positing in the
	 * `content` Map.
	 */
	given Index[Resource, String, Resource] =
		Index(key => Focus[Resource](_.as[Resource.Container].content.index(key)))

	/**
	 * typeclass for Index on a Container type S given a List of indexes.
	 * This is defined recursively.
	 * thanks to Ken Scrambler
	 */
	given [S, I](using Index[S, I, S]): Index[S, List[I], S] =
		Index {
			case Nil => Focus[S]()
			case key :: keys => Focus[S](_.index(key).index(keys))
		}

	/**
	 * The HTTP methods that one can use to interact with a Container.
	 * See local <src/test/scala/server/ServerTests.scala> for usage.
	 * Docs to look at:
	 * [[https://www.optics.dev/Monocle/docs/focus monocle focus]]
	 */
	extension (server: Resource)(using Index[Resource, List[String], Resource])
		def GET(path: List[String]): Response =
			server.focus(_.index(path)).getOption match {
				case Some(res) => Response(200, res.summary)
				case None => Response(404,"Content could not be found")
			}

		def POST(path: List[String])(
			slug: String, newcontent: String|LDPC.type
		): (Resource, Response) =
			//we limit to Containers, though a POST to a resource would work if its content is a monoid
			val newCntr: Option[Resource] = server.focus(_.index(path).as[Resource.Container])
				.modifyOption{ (cntr: Resource.Container) =>
				val index: Map[String, Resource] = cntr.content
				val name = if index.get(slug).isEmpty then slug else s"${slug}_${counter.next()}"
				val newRes = newcontent match
					case LDPC => Resource.Container()
					case text: String => Resource.TextResource(text)
				cntr.copy(content=index.updated(name,newRes))
			}
			newCntr match
			case Some(newC : Resource.Container) => (newC, Response(200,"how do we pass the name of the new resource here?"))
			case Some(resource) => (server, Response(500, "internatl server erorr. Seem to be replacing root container with a resource"))
			case None =>       (server, Response(404, "container does not exist"))




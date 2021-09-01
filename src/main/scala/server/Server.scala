package server

import cats.data.NonEmptyList as NEL
import monocle.Lens
import monocle.Optional
import monocle.function.Index
import monocle.syntax.all.*

import java.time.{Clock, Instant}
import scala.collection.immutable.*

/**
 * The aim of this is to see what a very primitive web server using Lenses
 * would look like, to see if this can bring some extra conceptual clarity
 * to questions in Solid.
 * See [[https://gitlab.com/web-cats/CG/-/issues/28 Web Cats Lenses issue 28]].
 */
object Server {
	//for an intro to Monocle see the video https://twitter.com/bblfish/status/1413758476896153601
	// and the thread of references above it.
	// documentation https://www.optics.dev/Monocle/

	sealed trait Resource[T] {
		def content: T

		def created: Instant

		//def modified: Instant
	}

	val counter: Iterator[Int] = new Iterator[Int] {
		var c = 0
		override def hasNext: Boolean = true
		override def next(): Int = c+1
	}

	def now: Instant = Clock.systemUTC().instant()

	/**
	 * a very simplified but potentially extensible model of a ldp:Container
	 * to test a lens model of the web.
	 * (see [[https://gitlab.com/web-cats/CG/-/issues/28 Lenses and the Web]]).
	 *
	 * We can think of a web server as just a root container with a number of resources
	 * (which can themselves be containers), each named by a key of type String.
	 * A path of such strings gives a URL Path.
	 *
	 * Locating a resource is just a matter of following the hashmap hierarchy deeper
	 * and deeper inwards.
	 *
	 **/
	case class Container(
		content: Map[String, Resource[_]] = Map(),
		created: Instant = now
	) extends Resource[Map[String, Resource[_]]]

	// of course there could be many more Resources (eg. RDF Graphs, Pictures, etc)
	case class TextResource(
		content: String,
		created: Instant = now
	) extends Resource[String]

	case class Response(code: Int, content: String)

	val root = Container()
	object LDPC

//	val contents = Lens[Container, Map[String, Resource[_]]](_.content){ newmap =>
//		cont => cont.copy(content = newmap)
//	}

	type Path = List[String]
	/**
	 * An Index gives a means to accessing and changing an element of our Container Tree
	 * by path.
	 */
	val ci: Index[Container, Path, Resource[?]] = Index(path =>
		Optional[Container,Resource[_]]{cntr =>
			def get(pos: Container, remainingPath: Path): Option[Resource[?]] =
				remainingPath match
				case Nil => Some(pos)
				case head:: Nil => pos.content.get(head)
				case head::tail =>
					pos.content.get(head) match
					case Some(child: Container) => get(child, tail)
					//if it is not a container we can't go further
					case _ => None
			get(cntr,path)
		}{ newRes => root =>
		  //calling code needs to make sure that the root container is not replaced by a resource
			def replace(pos: Container, remainingPath: Path): Option[Resource[?]] =
				remainingPath match
					case Nil => Some(newRes)
					case head :: Nil =>
						//should check that container is empty first if it is to be replaced.
						val newMap = pos.content.updated(head,newRes)
						Some(pos.copy(content=newMap))
					case head :: tail =>
						pos.content.get(head) match
							case Some(child: Container) =>
								replace(child, tail).map{newR =>
									val newMap = pos.content.updated(head, newR)
									pos.copy(content=newMap)
								}
							case _ => None
			replace(root, path) match
				case ok@Some(container: Container) => container
				//we can't overwrite the root container
				case _ => root
		})

	extension(server: Container)
		def GET(path: List[String]): Response =
			def output: Option[Resource[?]] => Response =
				case Some(ctnr: Container) => Response(200, ctnr.content.keys.mkString("• ","\n• ","\n"))
				case Some(res) => Response(200, res.content.toString)
				case None => Response(404,"Content could not be found")

			output(ci.index(path).getOption(server))

      //clearly the response indicates we are dealing with a state monad with the state being the Container.
		def POST(path: List[String])(slug: String)(newcontent: String|LDPC.type): (Container,Response) =
			val mod: Container => Option[Container] = ci.index(path)
			  .modifyOption{ (res: Resource[?]) =>
				res match
				case c : Container =>
					val index: Map[String, Resource[_]] = c.content
					val name = if index.get(slug).isEmpty then slug else s"${slug}_${counter.next()}"
					val newRes = newcontent match
						case LDPC => Container()
						case text: String => TextResource(text)
					c.copy(content=index.updated(name,newRes))
				case r: Resource[?] => res //mhh we could append to resources if they are of the right type
			}
			mod(server) match
			case Some(newC) => (newC,Response(200,"how do we pass the name of the new resource here?"))
			case None =>    (server, Response(404, "container does not exist"))



}




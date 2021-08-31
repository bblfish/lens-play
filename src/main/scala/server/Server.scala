package server

import cats.data.NonEmptyList as NEL
import monocle.Lens
import monocle.Optional
import monocle.function.Index
import monocle.syntax.all.*

import java.time.{Clock, Instant}
import scala.collection.immutable.*

object Server {

	//for an intro to Monocle see the video https://twitter.com/bblfish/status/1413758476896153601
	// and the thread of references above it.
	// and the documentation https://www.optics.dev/Monocle/

	sealed trait Resource[T] {
		def content: T

		def created: Instant

		//def modified: Instant
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

//	val contents = Lens[Container, Map[String, Resource[_]]](_.content){ newmap =>
//		cont => cont.copy(content = newmap)
//	}

	//we can replace an element in a container but not the container itself, so
	//the path must be non-empty
	val ci: Index[Container, NEL[String], Resource[_]] = Index(path =>
		Optional[Container,Resource[_]]{cntr =>
			def get(pos: Container, remainingPath: NEL[String]): Option[Resource[?]] =
				remainingPath match
				case NEL(head, Nil) => pos.content.get(head)
				case NEL(head, h::tail) =>
					pos.content.get(head) match
					case Some(child: Container) => get(child, NEL(h,tail))
					//if it is not a container we can't go further, and otherwise
					case _ => None
			get(cntr,path)
		}{ newRes => root =>
			def replace(pos: Container, remainingPath: NEL[String]): Option[Container] =
				remainingPath match
					case NEL(head,Nil) =>
						//we should check not to replace a non-empty container
						val newMap = pos.content.updated(head,newRes)
						Some(pos.copy(content=newMap))
					case NEL(head, h::tail) =>
						pos.content.get(head) match
							case Some(child: Container) =>
								replace(child, NEL(h,tail)).map{newR =>
									val newMap = pos.content.updated(head, newR)
									pos.copy(content=newMap)
								}
							case _ => None
			replace(root, path).getOrElse(root)
		})

	// we should return the listing of the Map for a container
	def GET(server: Container)(path: List[String]): Response =
		def output: Option[Resource[?]] => Response =
			case Some(ctnr: Container) => Response(200, ctnr.content.keys.mkString("• ","\n• ","\n"))
			case Some(res) => Response(200, res.content.toString)
			case None => Response(404,"Content could not be found")
		if path.isEmpty then output(Some(server))
		else output(ci.index(NEL(path.head,path.tail)).getOption(server))

//	def POST(path: List[String], content: Resource[?]):

}




package server

import monocle.function.Index
import monocle.syntax.all.*
import monocle.{Focus, Lens, Optional}

import java.time.{Clock, Instant}
import scala.collection.immutable.*

/**
 * The aim of this is to see what a very primitive web server using Lenses
 * would look like, to see if this can bring some extra conceptual clarity
 * to questions in Solid.
 * See [[https://gitlab.com/web-cats/CG/-/issues/28 Web Cats Lenses issue 28]].
 *
 * Thanks to Ken Scambler for major simplifications using Enum and list based indexes
 * [[https://discord.com/channels/632277896739946517/882978685936877608/883198894350163989 in Typelevel Monocle Discord Channel]]
 *
 * For an intro to Monocle see the video https://twitter.com/bblfish/status/1413758476896153601
 * and the thread of references above it. Documentation https://www.optics.dev/Monocle/
 */
object Server:

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
	enum LDP:
		case Container(content: Map[String, LDP] = Map(), created: Instant = now)

		/* we currently only have a TextResource, but we could add a mime type and have
		the content as a binary string to cover all cases */
		case TextResource(content: String, created: Instant = now)

		def summary: String = this match
			case Container(content, _) => content.keys.mkString("- ", "\n- ", "\n")
			case TextResource(content, _) => content
	end LDP

	def now: Instant = Clock.systemUTC().instant()
	val slugCounter: Iterator[Int] = new Iterator[Int] {
		var c = 0
		override def hasNext: Boolean = true
		override def next(): Int = c + 1
	}

	/** We can think of a web server as just a root container with a number of resources
	 * (which can themselves be containers), each named by a key of type String.
	 * A path of such strings gives a URL Path. */
	val root = LDP.Container()

	object LDPC

	case class Response(code: Int, content: String)

	type Path = List[String]

	/**
	 * typeclass for a basic Index on a Container, locates it's positing in the
	 * `content` Map.
	 */
	given Index[LDP, String, LDP] =
		Index(key => Focus[LDP](_.as[LDP.Container].content.index(key)))

	/** typeclass for Index on a Container type S given a List of indexes.
	 * This is defined recursively.
	 * thanks to Ken Scambler */
	given [S, I](using Index[S, I, S]): Index[S, List[I], S] =
		Index {
			case Nil => Focus[S]()
			case key :: keys => Focus[S](_.index(key).index(keys))
		}

	/** The HTTP methods that one can use to interact with a Container.
	 * See local <src/test/scala/server/ServerTests.scala> for usage.
	 * Docs to look at:
	 * [[https://www.optics.dev/Monocle/docs/focus monocle focus]]  */
	extension (server: LDP)(using Index[LDP, List[String], LDP])
		def GET(path: List[String]): Response =
			server.focus(_.index(path)).getOption match
			case Some(res) => Response(200, res.summary)
			case None => Response(404, "Content could not be found")

		def POST(path: List[String])(
			slug: String, newcontent: String | LDPC.type
		): (LDP, Response) =
			//we limit to Containers, though a POST to a resource would work if its content is a monoid
			val newCntr: Option[LDP] = server.focus(_.index(path).as[LDP.Container])
				.modifyOption { (cntr: LDP.Container) =>
					val index: Map[String, LDP] = cntr.content
					val name = if index.get(slug).isEmpty then slug else s"${slug}_${slugCounter.next()}"
					val newRes = newcontent match
						case LDPC => LDP.Container()
						case text: String => LDP.TextResource(text)
					cntr.copy(content = index.updated(name, newRes))
				}
			newCntr match
			case Some(newC: LDP.Container) => (newC, Response(200, "how do we pass the name of the new resource here?"))
			case Some(resource) => (server, Response(500, "internatl server erorr. Seem to be replacing root container with a resource"))
			case None => (server, Response(404, "container does not exist"))

end Server



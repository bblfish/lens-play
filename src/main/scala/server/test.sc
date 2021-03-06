import monocle.syntax.all._

import cats.data.NonEmptyList as NEL
import server.Server
import Server.{given,*}

val Some(r1) = root.focus(_.index(Nil).as[LDP.Container]).modifyOption{ (cntr: LDP.Container) =>
	val newMap = cntr.content.updated("hello",LDP.TextResource("hello"))
	cntr.copy(content=newMap)
}

r1.focus(_.index(List("hello"))).getOption

r1.GET(List("hello"))



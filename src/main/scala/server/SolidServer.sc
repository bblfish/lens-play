import monocle.syntax.all.*
import cats.data.NonEmptyList as NEL
import server.SolidServer.{given,*}

import Solid.*
import Path.*
import Segment as S
import monocle.syntax.all.*
import monocle.Iso

val s = S("hello")
val rt = Iso.id[Container]
val p: CPath = CPath(List(S("foo"),S("bar")))
val rt1: Container = rt.at(p).replace(Some(Container(nonCR= Map("readme"->NonCR("hello World")))))(root)
val p2: FPath = FPath("readme.first",CPath(List(S("foo"))))
val rt2 = rt.at(p2).replace(Some(NonCR("hello from Garmisch")))(rt1)

rt2.GET(p2)
rt2.GET(p)

val (rt3: Container,resp) = rt2.POST(p)("blog1","My first blog entry")
rt3.GET(FPath("blog1",p))

rt3.DELETE(FPath("blog1",p))
rt3.DELETE(p)
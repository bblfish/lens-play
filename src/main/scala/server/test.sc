import cats.data.NonEmptyList as NEL
import server.Server
import Server.*

root.GET(List())
val (v1,res) = root.POST(Nil)("hello")("world")
v1.GET(Nil)
res
val (v2,res2) = v1.POST(Nil)("container1")(LDPC)
val (v3, res3) = v2.POST(List("container1"))("readme.txt")("enter foo related content here")
v3.GET(List("container1","readme.txt"))

val readmeNoExistent = v3.GET(List("container1","notcreated.txt"))

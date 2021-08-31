import cats.data.NonEmptyList as NEL
import server.Server
import Server.*



val foo = ci.index(NEL.of("hello.txt"))
val repl = foo.replace(TextResource("hello world"))
val v1 = repl(Server.root)
val v2 = ci.index(NEL.of("foo")).replace(Container())(v1)
val v3 = ci.index(NEL.of("foo","readme.md")).replace(TextResource("first entry"))(v2)
val v4 = ci.index(NEL.of("foo","bar","baz")).replace(TextResource("an entry"))(v3)
v3 == v4

Server.GET(v4)(List())
Server.GET(v4)(List("foo","bar","baz"))
Server.GET(v4)(List("foo","readme.md"))

//val p1 = Server.POST("blog1","My first blog. Testing")
//val newRoot = p1(Server.root)
//


package server

class ServerTests extends munit.FunSuite {
	import server.Server
	import Server.*

	test("build a server") {
		val rootContent = root.GET(List())
		assertEquals(rootContent.code, 200)
		assertEquals(rootContent.content, "• \n")

		val (v1,res) = root.POST(Nil)("hello")("world")
		val rootV1Content = v1.GET(Nil)
		assertEquals(rootV1Content.code, 200)
		assertEquals(rootV1Content.content, "• hello\n")

		//we create a container by passing the LDPC object instead of a string
		val (v2,res2) = v1.POST(Nil)("container1")(LDPC)
		val rootV2Content = v2.GET(Nil)
		assertEquals(rootV2Content.code, 200)
		assertEquals(rootV2Content.content, "• hello\n• container1\n")

		//we create a readme.txt in `container1`
		val (v3, res3) = v2.POST(List("container1"))("readme.txt")("enter foo related content here")
		val readmeContent = v3.GET(List("container1","readme.txt"))
		assertEquals(readmeContent.code,200)
		assertEquals(readmeContent.content,"enter foo related content here")

		val readmeNoExistent = v3.GET(List("container1","notcreated.txt"))
		assertEquals(readmeNoExistent.code,404)
	}
}

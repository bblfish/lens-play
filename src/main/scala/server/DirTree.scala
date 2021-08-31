package server

import cats.free.Free


//object DirTree {
//   // see Runar's paper [[http://days2012.scala-lang.org/sites/days2012/files/bjarnason_trampolines.pdf on Trampolines]]
//	type Indexed[R] = Map[String,R]
//	// here we try out another Free combination
//	type DirTree[A] =  Free[Indexed, A]
//
//	val intmap: Indexed[Int] = Map("1" -> 1, "2" -> 2, "3" -> 3)
//
//	val todo: Free[Indexed, String] = Free.pure[Indexed,String]("buy tomatoes")
//	// mhh this is not going where I was hoping it would.
//	val root: Free[Indexed, String] = Free.liftF(Map("todo"->"buy tomatoes")
//
//
//}

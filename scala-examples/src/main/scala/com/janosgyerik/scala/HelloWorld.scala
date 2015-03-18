package com.janosgyerik.scala

object HelloWorld {
  def main(args: Array[String]) {
    println("Hello, world!")
  }
}
/*
 * scala> com.example.scala.HelloWorld.main(Array())
Hello, world!

scala> Range(1,5)
* 
* 
scala> com.example.scala.HelloWorld.main(Array())
Hello, world!

scala> Range(1,5)
res5: scala.collection.immutable.Range = Range(1, 2, 3, 4)

scala> Range(1, 10).filter(_ % 3 == 0)
res6: scala.collection.immutable.IndexedSeq[Int] = Vector(3, 6, 9)

scala> res6.map(x => 3 * x + 1)
res7: scala.collection.immutable.IndexedSeq[Int] = Vector(10, 19, 28)
*/
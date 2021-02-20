package com.evolutiongaming.bootcamp.typeclass

object TypeclassTask extends App {
  // Create type class -> class with type parameter
  // and pass type to method param
  trait HashCode[T] {
    def hash(x: T): Int
  }

  // Summoner implementation:
  // apply method with implicit type-class param
  // return implicit param
  object HashCode {
    def apply[T](implicit instance: HashCode[T]): HashCode[T] = instance
  }

  // Add syntax:
  // type as constructor argument
  // method name == syntax
  // implicit type-class as method argument
  // when have implicit param for type-class then call method from type-class with constructor arg
  implicit class HashCodeSyntax[A](x: A) {
    def hash(implicit instance: HashCode[A]): Int =
      instance.hash(x)
  }

  // Define implicit type-class instance for String
  implicit val hashLikeString: HashCode[String] = x => x.hashCode

  val res = "abc".hash
  println(res)
}

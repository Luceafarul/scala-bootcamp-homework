package com.evolutiongaming.bootcamp.typeclass

object Task1 {
  final case class Money(amount: BigDecimal)

  implicit val moneyOrdering: Ordering[Money] = (x: Money, y: Money) => x.amount.compare(y.amount)
}

object Task2 {
  trait Show[T] {
    def show(entity: T): String
  }
  object Show {
    def apply[T](implicit instance: Show[T]): Show[T] = instance
  }

  // Create syntax for Show so i can do User("1", "Oleg").show
  implicit class ShowSyntax[T](x: T) {
    def show(implicit instance: Show[T]): String = instance.show(x)

    // TODO: how to rewrite this example with using implicitly?
    // def show[T]: String = {
    //   val instance = implicitly[Show[T]]
    //   instance.show(x)
    // }
  }

  // Create Show instance for User
  implicit object ShowUser extends Show[User] {
    override def show(entity: User): String = s"User with name: ${entity.name} and id: ${entity.id}"
  }

  final case class User(id: String, name: String)

  User("1", "Oleg").show
}

object Task3 {
  type Error = String

  trait Parse[T] { // invent any format you want or it can be csv string
    def parse(entity: String): Either[Error, T]
  }
  object Parse {
    def apply[T](implicit instance: Parse[T]): Parse[T] = instance
  }

  implicit class ParseSyntax(x: String) {
    def parse[A](implicit instance: Parse[A]): Either[Error, A] = instance.parse(x)
  }

  // Create Parse instance for User
  implicit val parseUser: Parse[User] = (entity: String) => {
    entity.split(",").toList match {
      case id :: name :: Nil => Right(User(id, name))
      case _ => Left(s"Wrong format: $entity, using csv format: id,name")
    }
  }

  final case class User(id: String, name: String)

  // Create syntax for Parse so i can do "lalala".parse[User]
  // (and get an error because it is obviously not a User)
  "ohmygod".parse[User]
  "1,Lora".parse[User]
}

object Task4 extends App {

  // Design a typesafe equals so i can do a === b,
  // but it won't compile if a and b are of different types
  // define the typeclass (think of a method signature)
  // remember `a method b` is `a.method(b)`
  trait Eq[T] {
    def equals(a: T, b: T): Boolean
  }

  object Eq {
    def apply[T](implicit instance: Eq[T]): Eq[T] = instance
  }

  implicit class EqSyntax[T](a: T) {
    def ===(b: T)(implicit instance: Eq[T]): Boolean =
      instance.equals(a, b)
  }

  implicit val userEq: Eq[User] = (a: User, b: User) => a.id.equals(b.id)

  final case class User(id: String, name: String)

  User("1", "Lora") === User("2", "Lora") // false
  User("1", "Lora") === User("1", "Lora") // true
  //  User("1", "Lora") === "Lora" // compile error
}

object AdvancedHomework {
  // TODO: create a typeclass for flatMap method
}

package com.evolutiongaming.bootcamp.db

import cats.effect._
import doobie._
import doobie.implicits._

import java.time.Year
import java.util.UUID

object Database {
  final case class Books(sessionPool: Resource[IO, Transactor[IO]]) {
    implicit val uuidMeta: Meta[UUID] = Meta[String].timap(UUID.fromString)(_.toString)
    implicit val yearMeta: Meta[Year] = Meta[Int].timap(Year.of)(_.getValue)

    private val selectAll = sql"SELECT * FROM books".query[Book]

    def findAll(): IO[List[Book]] =
      sessionPool.use { tx => selectAll.to[List].transact(tx) }
  }
}

package com.evolutiongaming.bootcamp.db

import doobie.Meta
import io.circe.generic._

import java.time.{LocalDate, Year}
import java.util.UUID

@JsonCodec case class Author(id: UUID, name: String, birthday: LocalDate)

@JsonCodec final case class Book(id: UUID, authorId: UUID, title: String, year: Year, genre: String)

@JsonCodec final case class BookWithAuthor(id: UUID, author: Author, title: String, year: Year) {
  override def toString: String = s"$title ($year) by ${author.name}"
}

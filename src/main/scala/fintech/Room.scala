package fintech
import java.io.{BufferedReader, InputStreamReader}
import java.util.UUID

import cats.effect.{ContextShift, IO}
import io.circe.Decoder
import cats.implicits._

import scala.collection.JavaConverters._
import io.circe.parser._

import scala.concurrent.ExecutionContext

trait Room[A] {
  def all: IO[List[A]]

  def getById(id: UUID): IO[Option[A]]

  def byId(id: UUID): IO[A]

  def getUsersById(id: UUID): IO[Option[List[User]]]
}

object Room {

  private class ListRooms[A: Key](rooms: List[A], byIdMap: Map[UUID, A]) extends Room[A] {
    def all: IO[List[A]] = IO.pure(rooms)
    def getById(id: UUID): IO[Option[A]] = IO.pure(byIdMap.get(id))
    def byId(id: UUID): IO[A] = getById(id).flatMap(_.liftTo[IO](Key.notFound[A](id)))
    def getUsersById(id: UUID): IO[Option[List[User]]] = IO.pure(Option(byIdMap(id).asInstanceOf[Group].users))
  }

  private def makeListRooms[A: Key](groups: List[A]): IO[ListRooms[A]] =
    groups
      .groupBy(Key.key[A])
      .toList
      .traverse {
        case (id, List(a)) => println(a.asInstanceOf[Group].users); IO.pure(id -> a)
        case (id, _) => IO.raiseError(Key.multiple[A](id))
      }.map(byId => new ListRooms(groups, byId.toMap))

  def fromResource[A: Key : Decoder](name: String, blocking: ExecutionContext)(
    implicit basic: ContextShift[IO]): IO[Room[A]] = {
    val read: IO[List[A]] = IO {
      val stream = getClass.getResourceAsStream(name)
      val reader = new BufferedReader(new InputStreamReader(stream))
      val total = reader.lines().iterator.asScala.mkString
      decode[List[A]](total): Either[Throwable, List[A]]
    }.rethrow

    for {
      _ <- IO.shift(blocking)
      rooms <- read
      list <- makeListRooms(rooms)
      _ <- IO.shift(basic)
    } yield list
  }
}

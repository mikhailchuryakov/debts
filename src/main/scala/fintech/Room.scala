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

  def getUsersById(id: UUID): IO[List[User]]

  def createGroup(createGroup: CreateGroup): IO[Group]

  def createUser(groupID: UUID, createUser: CreateUser): IO[User]

  def removeUser(id: UUID)(userID: UUID): IO[Unit]
}

object Room {

  private class ListRooms[A: Key](rooms: List[A], byIdMap: Map[UUID, A]) extends Room[A] {
    def all: IO[List[A]] = IO.pure(rooms)
    def getById(id: UUID): IO[Option[A]] = IO.pure(byIdMap.get(id))
    def byId(id: UUID): IO[A] = getById(id).flatMap(_.liftTo[IO](Key.notFound[A](id)))
    def getUsersById(id: UUID): IO[List[User]] = IO.pure(byIdMap.get(id) match {
      case Some(a) => a.asInstanceOf[Group].users
      case _ => Nil
    })

    def createGroup(createGroup: CreateGroup): IO[Group] = {
      IO {
        new Group(UUID.randomUUID(), createGroup.name, List.empty)
      }
    }

    def createUser(groupID: UUID, createUser: CreateUser): IO[User] = {
      IO {
        ??? //new User(UUID.randomUUID(), createUser.name)
      }
    }

    def removeUser(id: UUID)(userID: UUID): IO[Unit] = ???
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

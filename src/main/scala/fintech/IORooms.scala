package fintech

import java.util.UUID

import cats.data.OptionT
import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import fintech.IORooms.State
import cats.effect.{ContextShift, IO}
import io.circe.Decoder
import cats.implicits._

import scala.collection.JavaConverters._
import io.circe.parser._

import scala.concurrent.ExecutionContext


trait Rooms {
  def all(): IO[List[Group]]

  def createGroup(name: String): IO[Group]

  def removeGroup(id: UUID): IO[Unit]

  def get(id: UUID): IO[Group]

  def putUser(group: UUID, name: String): IO[Unit]

  def removeUser(group: UUID, user: UUID): IO[Unit]
}

object Rooms {}

final case class IORooms(state: Ref[IO, State]) extends Rooms {
  def all(): IO[List[Group]] = ???

  def createGroup(name: String): IO[Group] = {
    for {
      id <- IO(UUID.randomUUID())
      ref <- Ref[IO].of(Group(id, name, List()))
      res <- state.modify(_.addNewGroupRef(id, name, ref))
    } yield {
      println(state.get.unsafeRunSync().byID) // todo delete
      res
    }
  }.flatMap(_.get)

  def removeGroup(id: UUID): IO[Unit] = ??? // todo remove group

  private def getRef(id: UUID): IO[Ref[IO, Group]] = {
    println(state.get.unsafeRunSync().byID) // todo delete
    state.get.flatMap(_.byID.get(id).liftTo[IO](GroupNotFound(id)))
  }

  private def updateRef(id: UUID)(f: Group => Group): IO[Unit] = getRef(id).flatMap(_.update(f))

  def get(id: UUID): IO[Group] = getRef(id).flatMap(_.get)

  def putUser(groupID: UUID, name: String): IO[Unit] =
    updateRef(groupID)(_.addUser(name))

  def removeUser(groupID: UUID, userID: UUID): IO[Unit] =
    updateRef(groupID)(_.removeUser(userID))
}

object IORooms {

  final case class State(byID: Map[UUID, Ref[IO, Group]], byName: Map[String, Ref[IO, Group]]) {
    def addNewGroupRef(id: UUID, name: String, groupRef: Ref[IO, Group]): (State, Ref[IO, Group]) =
      byName.get(name) match {
        case Some(ref) => this -> ref
        case None => State(byID + (id -> groupRef), byName + (name -> groupRef)) -> groupRef
      }
  }

  def create(): IO[Rooms] =
    for (
      ref <- Ref[IO].of(State(Map(), Map()))
    ) yield {
      IORooms(ref)
    }
}
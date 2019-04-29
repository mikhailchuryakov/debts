package fintech.http

import java.util.UUID

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.{ContextShift, IO, Timer}
import io.circe.Encoder
import fintech.{CreateGroup, Room, Rooms}

import scala.concurrent.duration.{Duration, FiniteDuration}

object groupHttp {
  def route[A: Encoder](prefix: String, room: Room[A])(implicit timer: Timer[IO], cs: ContextShift[IO]): Route =
    pathPrefix(prefix)(
      {
        println("all")
        (get & path("all")) (complete(room.all))
      }
    ) ~
      //      (put & parameter("name")) {
      //        name => {
      //          complete(???)
      //          //          entity(as[CreateGroup]) {qwe =>
      //          //            val inserted = room.createGroup(qwe(name))
      //          //            complete(room.all + inserted.toString())
      //          //          }
      //          //          val group = room.createGroup(CreateGroup(name))
      //          //          complete(IO.pure(room.all.unsafeRunSync() + group.toString()))
      //        }
      //      } ~
      pathPrefix(prefix / JavaUUID)(
        id => {
          path("users")(complete(room.getUsersById(id))) ~
            complete(room.getById(id))
        }
      )
}

object roomHttp {
  def route(rooms: Rooms)(implicit timer: Timer[IO], cs: ContextShift[IO]): Route =
    pathPrefix("user")(
      (get & path("add") & parameter("id") & parameter("name")) {
        (id, name) => {
          val inserted = UUID.fromString(id)
          rooms.putUser(inserted, name).unsafeRunSync()
          complete(rooms.get(inserted))
        }
      } ~
        (get & path("remove") & parameter("id") & parameter("userid")) {
          (id, userid) => {
            rooms.removeUser(UUID.fromString(id), UUID.fromString(userid)).unsafeRunSync()
            complete(rooms.get(UUID.fromString(id)))
          }
        }
    ) ~
      pathPrefix("groups")(
        (get & path("add") & parameter("name")) {
          name => {
            complete(rooms.createGroup(name))
          }
        }
      ) ~
      pathPrefix("pay")(
        (get & parameter("id") & parameter("idfrom") & parameter("value") & parameter("idto")) {
          (id, idfrom, value, idto) => {
            rooms.payFromTo(UUID.fromString(id), UUID.fromString(idfrom), value.toDouble,
              idto.split(",").map(id => UUID.fromString(id))).unsafeRunSync()
            complete(rooms.get(UUID.fromString(id)))
          }
        }
      )
}

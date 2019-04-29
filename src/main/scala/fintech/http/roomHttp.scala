package fintech.http

import java.util.UUID

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.{ContextShift, IO, Timer}
import fintech.Rooms
import io.circe.Encoder

//object groupHttp {
//  def route[A: Encoder](prefix: String, room: Room[A])(implicit timer: Timer[IO], cs: ContextShift[IO]): Route =
//    pathPrefix(prefix)(
//      {
//        println("all")
//        (get & path("all")) (complete(room.all))
//      }
//    ) ~
//      pathPrefix(prefix / JavaUUID)(
//        id => {
//          path("users")(complete(room.getUsersById(id))) ~
//            complete(room.getById(id))
//        }
//      )
//}

// todo change requests
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
      pathPrefix("group")(
        (get & path("add") & parameter("name")) {
          name => {
            complete(rooms.createGroup(name))
          }
        }
      ) ~
      pathPrefix("pay")(
        (get &
          parameter("id") & parameter("idfrom") & parameter("value") & parameter("idto")) {
          (id, idFrom, value, idTo) => {
            rooms.payFromTo(UUID.fromString(id), UUID.fromString(idFrom), value.toDouble,
              idTo.split(",").map(id => UUID.fromString(id))).unsafeRunSync()
            complete(rooms.get(UUID.fromString(id)))
          }
        }
      )
}

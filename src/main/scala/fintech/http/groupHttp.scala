package fintech.http

import java.util.UUID

import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import cats.effect.{ContextShift, IO, Timer}
import io.circe.Encoder
import fintech.Room

import scala.concurrent.duration.{Duration, FiniteDuration}

object groupHttp {
  def route[A: Encoder](prefix: String, room: Room[A])(implicit timer: Timer[IO], cs: ContextShift[IO]): Route =
    pathPrefix(prefix)(
      (get & path("all")) (complete(room.all))
    ) ~
    pathPrefix(prefix / JavaUUID) (
      id => {
        path("users") (complete(room.getUsersById(id))) ~
        complete(room.getById(id))
      }
    )
}

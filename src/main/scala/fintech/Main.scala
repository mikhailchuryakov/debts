package fintech

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import cats.effect.{ExitCode, IO, IOApp, Resource}
import fintech.http.roomHttp

import scala.concurrent.ExecutionContext

object Main extends IOApp {
  def system: IO[ActorSystem] = IO(ActorSystem())

  def runServerAndSystem(route: Route)(implicit system: ActorSystem): IO[Unit] =
    for {
      binding <- IO.fromFuture(IO {
        implicit val mat = ActorMaterializer()
        Http().bindAndHandle(route, "0.0.0.0", 8080)
      })
      res <- IO(println(binding))
    } yield res

  val blocking: Resource[IO, ExecutionContext] =
    Resource
      .make(IO(Executors.newCachedThreadPool()))(exec => IO(exec.shutdown()))
      .map(ExecutionContext.fromExecutor)

  def run(args: List[String]): IO[ExitCode] =
    blocking.use { blockingCS =>
      for {
        //groups <- Room.fromResource[Group]("/groups.json", blockingCS)
        ioRoom <- IORooms.create()
        //groupRout = groupHttp.route("group", groups)
        ioRoomRoute = roomHttp.route(ioRoom)
        sys: ActorSystem <- system
        _ <- runServerAndSystem(ioRoomRoute)(sys)
        _ <- IO.never
      } yield ExitCode.Success
    }

}

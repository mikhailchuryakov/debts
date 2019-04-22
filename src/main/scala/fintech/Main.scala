package fintech

import java.util.concurrent.Executors

import akka.actor.ActorSystem
import akka.http.scaladsl.Http

import scala.concurrent.ExecutionContext
import cats.implicits._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import cats.effect.{ExitCode, IO, IOApp, Resource}
import fintech.http.groupHttp

object Main extends IOApp{
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
        group <- Room.fromResource[Group]("/groups.json", blockingCS)
        groupRout = groupHttp.route("group", group)
        sys: ActorSystem <- system
        _ <- runServerAndSystem(groupRout)(sys)
        _                           <- IO.never
      } yield ExitCode.Success
    }

}

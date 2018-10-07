package edu.gatech.dt87.multiverse.ui

import cats.effect.IO
import edu.gatech.dt87.multiverse.language.compiler.Compiler
import fs2.Stream
import fs2.StreamApp
import fs2.StreamApp.ExitCode
import org.http4s.dsl.impl.Root
import org.http4s.dsl.io._
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.{HttpService, StaticFile}
import org.http4s.server.blaze._
import scala.concurrent.ExecutionContext.Implicits.global

object Main extends StreamApp[IO] {

  private def service(server: Server): HttpService[IO] = {
    HttpService[IO] {

      case GET -> Root / "initial" =>
        Ok(server.initial())

      case GET -> Root / "satisfiableGoalSet" / stateId =>
        Ok(server.satisfiableGoalSet(stateId))

      case GET -> Root / "satisfyGoal" / stateId / goalId =>
        Ok(server.satisfyGoal(stateId, goalId))

      case GET -> Root =>
        PermanentRedirect(uri("/ui.html"))

      case request@GET -> path if request.pathInfo.startsWith("/") =>
        System.out.println(path)
        StaticFile.fromResource("/edu/gatech/dt87/multiverse/ui" + path, Some(request)).getOrElseF(NotFound())
    }
  }

  override def stream(argument: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {

    val source = io.Source.fromFile(argument(0)).mkString
    val parsed = Compiler.compile(source)
    val server: Option[Server] = parsed.map(tuple => Server(tuple._1, tuple._2))

    BlazeBuilder[IO]
      .bindHttp(8080, "localhost")
      .mountService(service(server.get), "/")
      .serve
  }


}
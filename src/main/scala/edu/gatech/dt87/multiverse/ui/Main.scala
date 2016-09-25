package edu.gatech.dt87.multiverse.ui

import edu.gatech.dt87.multiverse.language.compiler.Compiler
import org.http4s.HttpService
import org.http4s.dsl.{Root, _}
import org.http4s.server._
import org.http4s.server.staticcontent.ResourceService.Config
import org.http4s.server.blaze.BlazeBuilder

object Main extends Object {
  private val static = cachedResource(Config("/edu/gatech/dt87/multiverse/ui", ""))

  private def cachedResource(config: Config): HttpService = {
    val cachedConfig = config.copy(cacheStartegy = staticcontent.MemoryCache())
    staticcontent.resourceService(cachedConfig)
  }

  private def service(server: Server) = {
    HttpService {

      case request@GET -> Root / "initial" =>
        Ok(server.initial())

      case request@GET -> Root / "satisfiableGoalSet" / stateId =>
        Ok(server.satisfiableGoalSet(stateId))

      case request@GET -> Root / "satisfyGoal" / stateId / goalId =>
        Ok(server.satisfyGoal(stateId, goalId))

      case request@GET -> Root =>
        PermanentRedirect(uri("/ui.html"))

      case request@GET -> _ if request.pathInfo.startsWith("/") =>
        static(request)
    }
  }

  def main(argument: Array[String]): Unit = {

    if (argument.length == 1) {
      val source = io.Source.fromFile(argument(0)).mkString
      val parsed = Compiler.compile(source)
      val server = parsed.map((tuple) => Server(tuple._1, tuple._2))
      server.foreach(s =>
        BlazeBuilder
          .bindHttp(8080, "localhost")
          .mountService(service(s), "/")
          .run
          .awaitShutdown())

    } else {
      println("Please provide a file.")
    }
  }


}
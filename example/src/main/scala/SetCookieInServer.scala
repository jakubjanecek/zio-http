import zhttp.http._
import zhttp.service._
import zio._

object SetCookieInServer extends App {

  val app: HttpApp[Any, Nothing] = HttpApp.collect { case Method.GET -> Root / "cookie" =>
    Response.http(
      headers = List(
        Header.setCookie(
          ResponseCookie("x", "value", Some("Thu, 31 Oct 2021 07:28:00 GMT"), None, Some("/cookie"), true, true),
        ),
      ),
      content = HttpData.CompleteData(Chunk.fromArray("response".getBytes(HTTP_CHARSET))),
    )
  }

  // Run it like any simple app
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8090, app.silent).exitCode
}

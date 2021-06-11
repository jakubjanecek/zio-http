import zhttp.http.{Header, RequestCookie}
import zhttp.service.{ChannelFactory, Client, EventLoopGroup}
import zio._

object CookieInClient extends App {
  val env     = ChannelFactory.auto ++ EventLoopGroup.auto()
  val url     = "https://www.youtube.com/"
  val headers = List(
    Header.host("www.youtube.com"),
    Header.cookies(
      List(
        RequestCookie("GPS", "1"),
        RequestCookie("VISITOR_INFO1_LIVE", "gr3K1wi6VVo"),
      ),
    ),
  )

  val program = for {
    res <- Client.request(url, headers)
    _   <- console.putStrLn {
      res.headers.toString()
    }
  } yield ()

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = program.exitCode.provideCustomLayer(env)

}

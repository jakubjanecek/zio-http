package zhttp.http

import zio.=!=

import scala.annotation.{implicitAmbiguous, implicitNotFound}

sealed trait ResponseCookie2[+N, +C] { self =>

  import zhttp.http.ResponseCookie2._

  def ++[N1 >: N, N2, N3, C1 >: C, C2, C3](other: ResponseCookie2[N2, C2])(implicit
    @implicitNotFound("Name is already set once")
    n: CanConcat[N1, N2, N3],
    @implicitNotFound("Content is already set once")
    c: CanConcat[C1, C2, C3],
  ): ResponseCookie2[N3, C3] =
    Concat(self.asInstanceOf[ResponseCookie2[N3, C3]], other.asInstanceOf[ResponseCookie2[N3, C3]])

  def cookieConfig[N1 >: N, C1 >: C](
    s: Cookie = Cookie("", ""),
  )(implicit ev: N1 <:< String, ev2: C1 <:< String): Cookie =
    self match {
      case Concat(a, b)       => {
        a.cookieConfig(b.cookieConfig(s))
      }
      case Name(name)         => s.copy(name = name)
      case Content(content)   => s.copy(content = content)
      case Expires(expires)   => s.copy(expires = Some(expires))
      case Domain(domain)     => s.copy(domain = Some(domain))
      case Paths(path)        => s.copy(path = Some(path))
      case Secure(secure)     => s.copy(secure = secure)
      case HttpOnly(httpOnly) => s.copy(httpOnly = httpOnly)
    }
}
object ResponseCookie2 {
  sealed trait CanConcat[X, Y, A]
  object CanConcat {
    implicit def concatL[A]: CanConcat[A, Nothing, A]                = null
    implicit def concatR[A]: CanConcat[Nothing, A, A]                = null
    implicit def concatNothing: CanConcat[Nothing, Nothing, Nothing] = null
  }
  final case class Cookie(
    name: String,
    content: String,
    expires: Option[String] = None, //TODO date type change
    domain: Option[String] = None,
    path: Option[String] = None,
    secure: Boolean = false,
    httpOnly: Boolean = false,
  )

  private final case class Concat[N, C](self: ResponseCookie2[N, C], other: ResponseCookie2[N, C])
      extends ResponseCookie2[N, C]
  private final case class Name[N](name: N)            extends ResponseCookie2[N, Nothing]
  private final case class Content[C](content: String) extends ResponseCookie2[Nothing, C]
  private final case class Expires(expires: String)    extends ResponseCookie2[Nothing, Nothing]
  private final case class Domain(domain: String)      extends ResponseCookie2[Nothing, Nothing]
  private final case class Paths(path: String)         extends ResponseCookie2[Nothing, Nothing]
  private final case class Secure(secure: Boolean)     extends ResponseCookie2[Nothing, Nothing]
  private final case class HttpOnly(httpOnly: Boolean) extends ResponseCookie2[Nothing, Nothing]

  def name(name: String): ResponseCookie2[String, Nothing]           = ResponseCookie2.Name(name)
  def content(content: String): ResponseCookie2[Nothing, String]     = ResponseCookie2.Content(content)
  def expires(expires: String): ResponseCookie2[Nothing, Nothing]    = ResponseCookie2.Expires(expires)
  def domain(domain: String): ResponseCookie2[Nothing, Nothing]      = ResponseCookie2.Domain(domain)
  def path(path: String): ResponseCookie2[Nothing, Nothing]          = ResponseCookie2.Paths(path)
  def secure(secure: Boolean): ResponseCookie2[Nothing, Nothing]     = ResponseCookie2.Secure(secure)
  def httpOnly(httpOnly: Boolean): ResponseCookie2[Nothing, Nothing] = ResponseCookie2.HttpOnly(httpOnly)

  def asCookie[N, C](
    self: ResponseCookie2[N, C],
  )(implicit
    @implicitAmbiguous("Name is not set") evN: N =!= Nothing,
    @implicitAmbiguous("Content is not set") evC: C =!= Nothing,
  ): ResponseCookie2[N, C] = self

}
object HelloWorld extends App {

//  val a = ResponseCookie2.name("s") ++ ResponseCookie2.content("s") ++
//    ResponseCookie2.path("s") ++ ResponseCookie2.expires("s") ++
//    ResponseCookie2.domain("s") ++ ResponseCookie2.secure(true) ++ ResponseCookie2.httpOnly(true) ++
//    ResponseCookie2.name("s")

  val b = ResponseCookie2.Cookie("s", "s")
  val c = (ResponseCookie2.name("ss") ++ ResponseCookie2.content("ss") ++ ResponseCookie2.domain(
    "ss",
  )).cookieConfig()
  println(c)
//  val e = ResponseCookie2.asCookie(ResponseCookie2.name("s") ++ ResponseCookie2.domain("s"))
}

// apply method
//constructor for creating cookie

package zhttp.http
import scala.annotation.implicitNotFound

sealed trait ResponseCookie2[+S] { self =>

  import zhttp.http.ResponseCookie2._

  def ++[S1 >: S](other: ResponseCookie2[S1]): ResponseCookie2[S1] =
    Concat(self, other)

  def cookieConfig[S1 >: S](s: Cookie)(implicit ev: S1 <:< String): Cookie =
    self match {
      case Concat(_, _)       => ???
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
  final case class Cookie(
    name: String,
    content: String,
    expires: Option[String] = None, //TODO date type change
    domain: Option[String] = None,
    path: Option[String] = None,
    secure: Boolean = false,
    httpOnly: Boolean = false,
  )

  private final case class Concat[S](self: ResponseCookie2[S], other: ResponseCookie2[S]) extends ResponseCookie2[S]
  private final case class Name[S](name: S)                                               extends ResponseCookie2[S]
  private final case class Content(content: String)                                       extends ResponseCookie2[Nothing]
  private final case class Expires(expires: String)                                       extends ResponseCookie2[Nothing]
  private final case class Domain(domain: String)                                         extends ResponseCookie2[Nothing]
  private final case class Paths(path: String)                                            extends ResponseCookie2[Nothing]
  private final case class Secure(secure: Boolean)                                        extends ResponseCookie2[Nothing]
  private final case class HttpOnly(httpOnly: Boolean)                                    extends ResponseCookie2[Nothing]

  def name(name: String): ResponseCookie2[String]           = ResponseCookie2.Name(name)
  def content(content: String): ResponseCookie2[Nothing]    = ResponseCookie2.Content(content)
  def expires(expires: String): ResponseCookie2[Nothing]    = ResponseCookie2.Expires(expires)
  def domain(domain: String): ResponseCookie2[Nothing]      = ResponseCookie2.Domain(domain)
  def path(path: String): ResponseCookie2[Nothing]          = ResponseCookie2.Paths(path)
  def secure(secure: Boolean): ResponseCookie2[Nothing]     = ResponseCookie2.Secure(secure)
  def httpOnly(httpOnly: Boolean): ResponseCookie2[Nothing] = ResponseCookie2.HttpOnly(httpOnly)

  @implicitNotFound("Cookie doesn't have name set")
  sealed trait HasName[S]
  implicit object HasName extends HasName[String]

  def asCookie[S](self: ResponseCookie2[S])(implicit ev: HasName[S]): ResponseCookie2[S] = ???

}
object HelloWorld extends App {

//  val a = ResponseCookie2.name("s") ++ ResponseCookie2.content("s") ++
//    ResponseCookie2.path("s") ++ ResponseCookie2.expires("s") ++
//    ResponseCookie2.domain("s") ++ ResponseCookie2.secure(true) ++ ResponseCookie2.httpOnly(true) ++
//    ResponseCookie2.name("s")

  val b = ResponseCookie2.Cookie("s", "s")
  val c = ResponseCookie2.asCookie(ResponseCookie2.content("s"))
  println(c)
}

// apply method
//constructor for creating cookie

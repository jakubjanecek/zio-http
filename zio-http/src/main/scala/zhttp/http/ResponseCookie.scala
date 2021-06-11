package zhttp.http

final case class ResponseCookie(
  name: String,
  content: String,
  expires: Option[String] = None, //TODO date type change
  domain: Option[String] = None,
  path: Option[String] = None,
  secure: Boolean = false,
  httpOnly: Boolean = false,
)                     { self =>
  def clearCookie: ResponseCookie =
    copy(content = "", expires = None)

  private def getExpire(): String   = self.expires match {
    case Some(value) => s"; Expires=$value"
    case None        => ""
  }
  private def getDomain(): String   = self.domain match {
    case Some(value) => s"; Domain=$value"
    case None        => ""
  }
  private def getPath(): String     = self.path match {
    case Some(value) => s"; Path=$value"
    case None        => ""
  }
  private def getSecure(): String   = if (self.secure) "; Secure" else ""
  private def getHttpOnly(): String = if (self.httpOnly) "; HttpOnly" else ""

  def cookieParser: String = {
    s"${self.name}=${self.content}${getExpire()}${getDomain()}${getPath()}${getSecure()}${getHttpOnly()}"
  }
}
object ResponseCookie {

  def stringToResponseCookie(value: String) = ???

  def toResponseCookie(header: Header): ResponseCookie = stringToResponseCookie(header.value.toString)
}

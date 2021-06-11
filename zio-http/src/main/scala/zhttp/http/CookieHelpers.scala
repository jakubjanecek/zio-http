package zhttp.http

import io.netty.handler.codec.http.HttpHeaderNames

private[zhttp] trait CookieHelpers { self: HasHeaders =>

  def getCookies(): List[ResponseCookie]              =
    self.headers.filter(x => x.name == HttpHeaderNames.SET_COOKIE).map(ResponseCookie.toResponseCookie)
  def getCookie(name: String): Option[ResponseCookie] = ???
}

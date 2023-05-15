[//]: # (This file was autogenerated using `zio-sbt-website` plugin via `sbt generateReadme` command.)
[//]: # (So please do not edit it manually. Instead, change "docs/index.md" file or sbt setting keys)
[//]: # (e.g. "readmeDocumentation" and "readmeSupport".)

# ZIO Http

ZIO Http is a scala library for building http apps. It is powered by ZIO and [netty](https://netty.io/) and aims at being the defacto solution for writing, highly scalable and performant web applications using idiomatic scala.

[![Development](https://img.shields.io/badge/Project%20Stage-Development-green.svg)](https://github.com/zio/zio/wiki/Project-Stages) ![CI Badge](https://github.com/zio/zio-http/workflows/Continuous%20Integration/badge.svg) [![Sonatype Releases](https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-http_2.13.svg?label=Sonatype%20Release)](https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-http_2.13/) [![Sonatype Snapshots](https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-http_2.13.svg?label=Sonatype%20Snapshot)](https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-http_2.13/) [![javadoc](https://javadoc.io/badge2/dev.zio/zio-http-docs_2.13/javadoc.svg)](https://javadoc.io/doc/dev.zio/zio-http-docs_2.13) [![ZIO Http](https://img.shields.io/github/stars/zio/zio-http?style=social)](https://github.com/zio/zio-http)

## Installation

Setup via `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-http" % "1.0.0.0-RC17"
```

**NOTES ON VERSIONING:**

- Older library versions `1.x` or `2.x` with organization `io.d11` of ZIO Http are derived from Dream11, the organization that donated ZIO Http to the ZIO organization in 2022. 
- Newer library versions, starting in 2023 and resulting from the ZIO organization (`dev.zio`) started with `0.0.x`, reaching `1.0.0` release candidates in April of 2023

## Getting Started

A simple Http server can be built using a few lines of code.

```scala
import zio._
import zio.http._

object HelloWorld extends ZIOAppDefault {

  val app: App[Any] = 
    Http.collect[Request] {
      case Method.GET -> Root / "text" => Response.text("Hello World!")
    }

  override val run =
    Server.serve(app).provide(Server.default)
}
```

## Steps to run an example

1. Edit the [RunSettings](https://github.com/zio/zio-http/blob/main/project/BuildHelper.scala#L107) - modify `className` to the example you'd like to run.
2. From sbt shell, run `~example/reStart`. You should see `Server started on port: 8080`.
3. Send curl request for defined `http Routes`, for eg : `curl -i "http://localhost:8080/text"` for `example.HelloWorld`.

## Watch Mode

You can use the [sbt-revolver] plugin to start the server and run it in watch mode using `~ reStart` command on the SBT console.

[sbt-revolver]: https://github.com/spray/sbt-revolver

## Documentation

Learn more on the [ZIO Http homepage](https://github.com/zio/zio-http)!

## Contributing

For the general guidelines, see ZIO [contributor's guide](https://zio.dev/about/contributing).

## Code of Conduct

See the [Code of Conduct](https://zio.dev/about/code-of-conduct)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].

[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"

## License

[License](LICENSE)

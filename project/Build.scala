import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "s2"
    val appVersion      = "1.0.19"

    val appDependencies = Seq(
      "net.liftweb" %% "lift-json" % "2.4-M5",
      "com.ning" % "async-http-client" % "1.7.0" withSources(),
      "mysql" % "mysql-connector-java" % "5.1.18",
      "joda-time" % "joda-time" % "2.0",
      "org.joda" % "joda-convert" % "1.1",
      "commons-lang" % "commons-lang" % "2.3",
      "org.specs2" %% "specs2" % "1.7.1" % "test",
      "org.scalaz" %% "scalaz-core" % "6.0.4" withSources()
  )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      defaultScalaSettings:_*
    )
}

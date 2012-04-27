import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

  val appName = "s2"
  val appVersion = "1.0.19"

  // Only compile the bootstrap bootstrap.less file and any other *.less file in the stylesheets directory
  def customLessEntryPoints(base: File): PathFinder = (
    (base / "app" / "assets" / "stylesheets" / "bootstrap" * "bootstrap.less") +++
      (base / "app" / "assets" / "stylesheets" * "*.less")
    )

  val appDependencies = Seq(
    "com.ning" % "async-http-client" % "1.7.0" withSources(),
    "mysql" % "mysql-connector-java" % "5.1.18",
    "joda-time" % "joda-time" % "2.0",
    "org.joda" % "joda-convert" % "1.1",
    "commons-lang" % "commons-lang" % "2.3",
    "org.specs2" %% "specs2" % "1.7.1" % "test",
    "org.scalaz" %% "scalaz-core" % "6.0.4" withSources()
  )

  //lazy val deploy = Project("deploy", file("deploy"))



  val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
    lessEntryPoints <<= baseDirectory(customLessEntryPoints)
  )
}
object HelloBuild extends Build {

  val getTimestampKey = TaskKey[String]("get-timestamp", "Gets the current time")


  val getTimestamp = getTimestampKey := { 
      val format = new java.text.SimpleDateFormat("yyyy_MM_dd_hh_mm_ss")
      format.format(new java.util.Date())
    }

    val sampleKeyA = SettingKey[String]("sample-a", "demo key A")
    val sampleKeyB = SettingKey[String]("sample-b", "demo key B")
    val sampleKeyC = SettingKey[String]("sample-c", "demo key C")
    val sampleKeyD = SettingKey[String]("sample-d", "demo key D")

    
    override lazy val settings = super.settings ++
        Seq(sampleKeyA := "A: in Build.settings in Build.scala", resolvers := Seq()) ++
        Seq(getTimestamp) ++
        Seq(sampleKeyB := "B: in Build.settings in Build.scala", resolvers := Seq()) 

    lazy val root = Project(id = "hello",
                            base = file("."),
                            settings = Project.defaultSettings ++ Seq(sampleKeyB := "B: in the root project settings in Build.scala"))
}


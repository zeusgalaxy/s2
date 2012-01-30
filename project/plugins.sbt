resolvers ++= Seq(
  DefaultMavenRepository,
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/",
  Resolver.file("Play Local", file("/usr/local/play-2.0-RC1/repository/local"))( Resolver.ivyStylePatterns),
  Resolver.file("Ivy Local", file("/Users/kenner/.ivy2/local"))( Resolver.ivyStylePatterns),
  Resolver.file("Ivy Cache", file("/Users/kenner/.ivy2/cache"))( Resolver.ivyStylePatterns)
)

addSbtPlugin("play" % "sbt-plugin" % "2.0-RC1-SNAPSHOT")
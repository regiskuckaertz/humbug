name := "thrift"

version := "0.1"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"    % "1.13.4" % "test",
  "org.scodec"     %% "scodec-stream" % "1.0.1",
  "com.chuusai"    %% "shapeless"     % "2.3.2"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

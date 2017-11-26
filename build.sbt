import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

name := "thrift"

version := "0.1"

scalaVersion := "2.12.3"

scalacOptions ++= Seq("-feature", "-language:higherKinds")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"    % "1.13.4" % "test",
  "org.scodec"     %% "scodec-bits"   % "1.1.5",
  "org.scodec"     %% "scodec-core"   % "1.10.3",
  "com.chuusai"    %% "shapeless"     % "2.3.2"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

val preferences =
  ScalariformKeys.preferences := ScalariformKeys.preferences.value
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DoubleIndentConstructorArguments, true)
    .setPreference(DanglingCloseParenthesis, Preserve)
    .setPreference(RewriteArrowSymbols, true)

Seq(preferences)
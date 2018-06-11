import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

name := "humbug"

version := "0.1"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature", "-language:higherKinds", "-Ypartial-unification", "-language:existentials")

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck"    % "1.13.4" % "test",
  "org.scodec"     %% "scodec-bits"   % "1.1.5",
  "org.scodec"     %% "scodec-core"   % "1.10.3",
  "org.typelevel"  %% "cats-core"     % "1.1.0"
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

Test / testOptions += Tests.Argument(
  TestFrameworks.ScalaCheck, 
  "-maxSize", "5", 
  "-minSuccessfulTests", "33", 
  "-workers", "1", 
  "-verbosity", "1"
)
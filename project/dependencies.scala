import sbt._

object Dependencies {
  val scalaTest = "org.scalatest" %% "scalatest" % "3.0.1" % "test"
  val junit =  "junit" % "junit" % "4.12" % "test"
  val scalaXML = "org.scala-lang.modules" %% "scala-xml" % "1.0.6"

  val testDependencies = Seq(scalaTest, junit)
}

import sbt._
import Keys._

object Zmpp2 extends Build {

  override lazy val settings = super.settings ++ buildSettings

  def buildSettings = Seq(
    organization := "org.zmpp",
    version := "1.0",
    scalaVersion := "2.9.1",
    javacOptions in Compile ++= Seq("-target", "6", "-source", "6")
  )

  lazy val root = Project("root", file(".")) aggregate(zmpp_swing, zmpp_zcode, zmpp_tads3)
  lazy val zmpp_swing = Project("zmpp-swing", file("zmpp-swing")) dependsOn(common, glulx, glk)
  lazy val zmpp_zcode = Project("zmpp-zcode", file("zmpp-zcode")) settings(testDependencies :_*) dependsOn(common)
  lazy val zmpp_tads3 = Project("zmpp-tads3", file("zmpp-tads3")) settings(testDependencies :_*) dependsOn(common)
  lazy val glulx = Project("zmpp-glulx", file("zmpp-glulx")) dependsOn(common, glk)
  lazy val glk = Project("zmpp-glk", file("zmpp-glk")) dependsOn(common)
  lazy val common = Project("zmpp-common", file("zmpp-common"))

  def testDependencies = libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.6.1", "junit" % "junit" % "4.9")
}


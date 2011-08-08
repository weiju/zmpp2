import sbt._
import Keys._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) aggregate(zmpp_swing, zmpp_zcode, zmpp_tads3)
  lazy val zmpp_swing = Project("zmpp-swing", file("zmpp-swing")) settings(scalaVersion := "2.9.0-1") dependsOn(common, glulx, glk)
  lazy val zmpp_zcode = Project("zmpp-zcode", file("zmpp-zcode")) settings(scalaVersion := "2.9.0-1") dependsOn(common)
  lazy val zmpp_tads3 = Project("zmpp-tads3", file("zmpp-tads3")) settings(scalaVersion := "2.9.0-1") dependsOn(common)
  lazy val glulx = Project("zmpp-glulx", file("zmpp-glulx")) settings(scalaVersion := "2.9.0-1") dependsOn(common, glk)
  lazy val glk = Project("zmpp-glk", file("zmpp-glk")) settings(scalaVersion := "2.9.0-1") dependsOn(common)
  lazy val common = Project("zmpp-common", file("zmpp-common")) settings(scalaVersion := "2.9.0-1")
}


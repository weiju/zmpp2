import Dependencies._

name := "zmpp2"

organization := "org.zmpp"

version := "1.0"

scalaVersion in ThisBuild := "2.11.8"

javacOptions in ThisBuild ++= Seq("-source", "1.6", "-target", "1.6")

lazy val swing = (project in file("zmpp-swing")).settings(libraryDependencies ++= testDependencies).dependsOn(common, glulx, glk)

lazy val zcode = (project in file("zmpp-zcode")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val tads3 = (project in file("zmpp-tads3")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val glulx = (project in file("zmpp-glulx")).settings(libraryDependencies ++= testDependencies).dependsOn(common, glk)

lazy val glk = (project in file("zmpp-glk")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val common = (project in file("zmpp-common")).settings(libraryDependencies ++= testDependencies)

lazy val root = (project in file(".")).aggregate(swing, zcode, tads3)

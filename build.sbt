import Dependencies._

name := "zmpp2"

organization := "org.zmpp"

version := "1.0"

scalaVersion := "2.12.1"

lazy val zmpp_swing = (project in file("zmpp-swing")).settings(libraryDependencies ++= testDependencies).dependsOn(common, glulx, glk)

lazy val zmpp_zcode = (project in file("zmpp-zcode")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val zmpp_tads3 = (project in file("zmpp-tads3")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val glulx = (project in file("zmpp-glulx")).settings(libraryDependencies ++= testDependencies).dependsOn(common, glk)

lazy val glk = (project in file("zmpp-glk")).settings(libraryDependencies ++= testDependencies).dependsOn(common)

lazy val common = (project in file("zmpp-common")).settings(libraryDependencies ++= testDependencies)

lazy val root = (project in file(".")).aggregate(zmpp_swing, zmpp_zcode, zmpp_tads3)

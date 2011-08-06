import sbt._

object MyBuild extends Build {

  lazy val root = Project("root", file(".")) aggregate(zmpp_swing, zmpp_zcode, zmpp_tads3)
  lazy val zmpp_swing = Project("zmpp-swing", file("zmpp-swing")) dependsOn(common, glulx, glk)
  lazy val zmpp_zcode = Project("zmpp-zcode", file("zmpp-zcode")) dependsOn(common)
  lazy val zmpp_tads3 = Project("zmpp-tads3", file("zmpp-tads3")) dependsOn(common)
  lazy val glulx = Project("zmpp-glulx", file("zmpp-glulx")) dependsOn(common, glk)
  lazy val glk = Project("zmpp-glk", file("zmpp-glk")) dependsOn(common)
  lazy val common = Project("zmpp-common", file("zmpp-common"))
}


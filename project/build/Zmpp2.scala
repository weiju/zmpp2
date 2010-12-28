import sbt._

class Zmpp2Project(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test"
  lazy val common = project("zmpp-common", "zmpp-common")
  lazy val glk    = project("zmpp-glk", "zmpp-glk", common)
  lazy val glulx  = project("zmpp-glulx", "zmpp-glulx", common, glk)
  lazy val tads3  = project("zmpp-tads3", "zmpp-tads3", common)
  lazy val zcode  = project("zmpp-zcode", "zmpp-zcode", common)
  lazy val zmpp_swing  = project("zmpp-swing", "zmpp-swing",
                                 glulx, glk, common)
}

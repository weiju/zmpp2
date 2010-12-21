import sbt._

class Zmpp2Project(info: ProjectInfo) extends DefaultProject(info) {
  val specs = "org.scala-tools.testing" % "specs_2.8.0" % "1.6.5" % "test"
  lazy val common = project("zmpp-common", "ZMPP common classes")
  lazy val glk    = project("zmpp-glk", "ZMPP Glk classes", common)
  lazy val glulx  = project("zmpp-glulx", "ZMPP Glulx classes", common, glk)
  lazy val tads3  = project("zmpp-tads3", "ZMPP TADS3 classes", common)
  lazy val zcode  = project("zmpp-zcode", "ZMPP Z-code classes", common)
  lazy val zmpp_swing  = project("zmpp-swing", "ZMPP Swing UI",
                                 glulx, glk, common)
}

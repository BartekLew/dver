import sbt.Keys._
import sbt._

object BuildSettings {

  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "me.leo",
    version := "0.0",
    scalaVersion := "2.11.6"
  )

}

#!/bin/sh
exec scala -savecompiled -deprecation "$0" "$@"
!#

import scala.sys.process._

val versionFile = "src/main/resources/version.txt"
val currentVersion = io.Source.fromFile(versionFile).getLines().next().toInt
val newVersion = currentVersion + 1
val versionFos = new java.io.FileOutputStream(versionFile)
versionFos.write(s"$newVersion".getBytes("UTF-8"))
println(List("mvn", "versions:set", s"-DnewVersion=$newVersion").!!)
println(s"$currentVersion -> $newVersion")
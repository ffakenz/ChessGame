name := "ChessGame"

version := "0.1"

scalaVersion := "2.13.3"

unmanagedJars in Compile += file("./ProgTest/libs/userinput.jar")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

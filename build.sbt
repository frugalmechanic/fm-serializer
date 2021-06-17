FMPublic

name := "fm-serializer"

description := "Scala Macro Based Serialization"

scalaVersion := "2.12.13"

crossScalaVersions := Seq("2.11.12", "2.12.13")

// Needed for the JavaBean tests to work
compileOrder := CompileOrder.JavaThenScala

// NOTE: For -Xelide-below:  ALL == Enabled Assertions,  OFF == Disabled Assertions
scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-language:implicitConversions,experimental.macros",
  "-feature",
  "-Xlint",
  "-Ywarn-unused-import",
  "-Xelide-below", "OFF"
) ++ (if (scalaVersion.value.startsWith("2.12")) Seq(
  // Scala 2.12 specific compiler flags
  "-opt:l:inline",
  "-opt-inline-from:<sources>"
) else Nil)

// Need to make sure any Java sources are compiled to 1.8 classfile format
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")

// We don't want log buffering when running ScalaTest
logBuffered in Test := false

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

// SCALA Libraries
libraryDependencies ++= Seq(
  "com.frugalmechanic" %% "fm-common" % "0.51.0",
  "com.frugalmechanic" %% "fm-json" % "0.5.0"
)

// JAVA Libraries
libraryDependencies ++= Seq(
  "com.sun.xml.bind" % "jaxb-core" % "2.3.0.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "com.sun.xml.bind" % "jaxb-impl" % "2.3.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "javax.xml.bind" % "jaxb-api" % "2.3.1", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "javax.activation" % "javax.activation-api" % "1.2.0", // JAXB - Needed for Java 9+ since it is no longer automatically available
  "joda-time" % "joda-time" % "2.9.1",
  "org.joda" % "joda-convert" % "1.8", // Required by joda-time when using Scala
  "org.mongodb" % "bson" % "3.3.0",
  "org.yaml" % "snakeyaml" % "1.23", // YAML => JSON
)

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

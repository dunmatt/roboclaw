lazy val commonSettings = Seq( organization := "com.github.dunmatt",
                               version := "0.1.0",
                               scalaVersion := "2.11.4")

resolvers += Resolver.mavenLocal
val managedDependencies = Seq(
)

lazy val root = (project in file(".")).settings( commonSettings: _*)
                                      .settings(
  name := "RoboClaw"
  , libraryDependencies ++= managedDependencies
)
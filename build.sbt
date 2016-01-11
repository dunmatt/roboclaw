lazy val commonSettings = Seq( organization := "com.github.dunmatt",
                               version := "0.2.4",
                               scalaVersion := "2.11.7")

resolvers += Resolver.mavenLocal
val managedDependencies = Seq(
  "com.squants" %% "squants" % "0.5.3"
)

lazy val root = (project in file(".")).settings( commonSettings: _*)
                                      .settings(
  name := "RoboClaw"
  , libraryDependencies ++= managedDependencies
)
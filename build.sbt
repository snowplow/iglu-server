
/**
* Currently we have some libs that depend on circe 0.14.x and some that depend on 0.13.x.
* These reported binary incompatibilities can only be removed once we have bumped cats-effect to version 3.
* For now, we ignore the reported binary incompatibilities because testing shows it is safe.
*/
ThisBuild / libraryDependencySchemes ++= Seq(
  "io.circe" %% "circe-jawn" % "always",
  "io.circe" %% "circe-core" % "always",
)

lazy val allSettings = BuildSettings.projectSettings ++
  BuildSettings.licenseSettings ++
  BuildSettings.buildInfoSettings ++
  BuildSettings.assemblySettings ++
  BuildSettings.dynVerSettings ++
  BuildSettings.testsSettings ++
  BuildSettings.compilerSettings ++
  (libraryDependencies ++= Dependencies.all)

lazy val root = project
  .settings(BuildSettings.projectSettings)
  .settings(BuildSettings.licenseSettings)
  .settings(BuildSettings.dynVerSettings)
  .settings(BuildSettings.compilerSettings)
  .aggregate(igluServer)

lazy val igluServer = (project in file("."))
  .settings(allSettings)
  .settings(BuildSettings.additionalDockerSettings)
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin, SnowplowDockerPlugin)

lazy val igluServerDistroless = (project in file("./distroless"))
  .settings(allSettings)
  .settings(BuildSettings.additionalDockerSettings)
  .settings(sourceDirectory := (igluServer / sourceDirectory).value)
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin, SnowplowDistrolessDockerPlugin)

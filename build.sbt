lazy val allSettings = BuildSettings.projectSettings ++
  BuildSettings.buildInfoSettings ++
  BuildSettings.dockerSettings ++
  BuildSettings.assemblySettings ++
  BuildSettings.dynVerSettings ++
  BuildSettings.testsSettings ++
  BuildSettings.compilerSettings ++
  (libraryDependencies ++= Dependencies.all)

lazy val root = project
  .settings(BuildSettings.projectSettings)
  .settings(BuildSettings.dynVerSettings)
  .settings(BuildSettings.compilerSettings)
  .aggregate(igluServer)

lazy val igluServer = (project in file("."))
  .settings(allSettings)
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin, SnowplowDockerPlugin)

lazy val igluServerDistroless = (project in file("./distroless"))
  .settings(allSettings)
  .settings(sourceDirectory := (igluServer / sourceDirectory).value)
  .enablePlugins(JavaAppPackaging, BuildInfoPlugin, SnowplowDistrolessDockerPlugin)
  .dependsOn(igluServer % "test->test;compile->compile")

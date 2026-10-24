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

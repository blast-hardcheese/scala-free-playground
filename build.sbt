name := "free-experiments"

scalaVersion := "2.11.8"

libraryDependencies += "org.typelevel" %% "cats" % "0.6.0"

scalacOptions += "-feature"

addCompilerPlugin("com.milessabin" % "si2712fix-plugin" % "1.2.0" cross CrossVersion.full)

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" % "kind-projector" % "0.8.0" cross CrossVersion.binary)

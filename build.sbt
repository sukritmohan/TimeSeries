import AssemblyKeys._

name := "Bears2Bulls"

version := "1.0"

scalaVersion := "2.10.1"

libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
            "org.scalanlp" %% "breeze-math" % "0.2",
            "org.scalanlp" %% "breeze-learn" % "0.2",
            "org.scalanlp" %% "breeze-process" % "0.2",
            "org.scalanlp" %% "breeze-viz" % "0.2",
	    "javax.mail" % "mail" % "1.4"
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.2-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

assemblySettings

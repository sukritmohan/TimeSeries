import AssemblyKeys._

name := "Bears2Bulls"

version := "1.0"

scalaVersion := "2.10.1"

//libraryDependencies += groupID % artifactID % revision

libraryDependencies  ++= Seq(
            // other dependencies here
            // pick and choose:
            // Breeze stuff
            "org.scalanlp" %% "breeze-math" % "0.2",
            "org.scalanlp" %% "breeze-learn" % "0.2",
            "org.scalanlp" %% "breeze-process" % "0.2",
            "org.scalanlp" %% "breeze-viz" % "0.2",
	    //mail stuff
	    "javax.mail" % "mail" % "1.4.5",
	    // jsoup - web parsing
	    "org.jsoup" % "jsoup" % "1.7.2",
	    // Jackson stuff
	    "com.fasterxml.jackson.core" % "jackson-core" % "2.2.2",
	    "com.fasterxml.jackson.core" % "jackson-databind" % "2.2.2",
	    "com.fasterxml.jackson.module" % "jackson-module-scala_2.10" % "2.2.1",
	    // Stanford NLP stuff (might need this later)
	    "edu.stanford.nlp" % "stanford-corenlp" % "1.3.5",
	    //Goose webpage extraction
	    "com.gravity" % "goose" % "2.1.22",
	    //HTTP-Client (for testing kayak download only)
		//org.apache.commons.httpclient._
	    "commons-httpclient" % "commons-httpclient" % "3.1"
)

resolvers ++= Seq(
            // other resolvers here
            // if you want to use snapshot builds (currently 0.2-SNAPSHOT), use this.
            "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/releases/"
)

assemblySettings

initialCommands := """
  import System.{currentTimeMillis => now}
  import utils._
  import dataUtils._
  import nlp._
  def time[T](f: => T): T = {
    val start = now
    try { f } finally { println("Elapsed: " + (now - start)/1000.0 + " s") }
  }
  import dataUtils.DataPreparation
"""


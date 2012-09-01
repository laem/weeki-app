import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "weeki"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "com.twitter" % "finagle-core" % "5.3.0",
      "com.twitter" % "finagle-http" % "5.3.0",
      //"com.twitter" % "finagle-stream" % "5.3.0",
      "com.codahale" % "jerkson_2.9.1" % "0.5.0",
      "com.twitter" % "joauth" % "1.9.3",
      "org.ccil.cowan.tagsoup" % "tagsoup" % "1.2.1"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here      
      resolvers += "twitter.com" at "http://maven.twttr.com/",
      resolvers += "repo.codahale.com" at "http://repo.codahale.com",
      resolvers += "Maven" at "http://repo1.maven.org/maven2"
    )

}

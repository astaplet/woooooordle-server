plugins {
    // Apply the scala Plugin to add support for Scala.
    scala

    // Apply the application plugin to add support for building a CLI application in Java.
    application
}

repositories {
    mavenCentral()
    jcenter()
}

dependencies {
    // Use Scala 2.13 in our library project
    implementation("org.scala-lang:scala-library:2.13.8")

    // This dependency is used by the application.
    implementation("com.google.guava:guava:30.1.1-jre")

    // Use Scalatest for testing our library
    testImplementation("junit:junit:4.13.2")
    testImplementation("org.scalatest:scalatest_2.13:3.2.10")
    testImplementation("org.scalatestplus:junit-4-13_2.13:3.2.2.0")

    // Need scala-xml at test runtime
    testRuntimeOnly("org.scala-lang.modules:scala-xml_2.13:1.2.0")

    //Caffeine cache for the GameStateActor
    implementation("com.github.blemale:scaffeine_2.13:5.1.2")

    //Akka

    implementation("com.typesafe.akka:akka-bom_2.13:2.6.18")

    implementation ("com.typesafe.akka:akka-actor_2.13:2.6.18")
    testImplementation ("com.typesafe.akka:akka-testkit_2.13:2.6.18")

    //command line parser
    implementation("org.sellmerfud:optparse_2.13:2.2")

}

application {
    // Define the main class for the application.
    mainClass.set("woooooordle.server.App")
}

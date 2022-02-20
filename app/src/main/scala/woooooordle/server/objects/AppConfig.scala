package woooooordle.server.objects

import akka.util.Timeout
import com.typesafe.config.ConfigFactory

import java.util.concurrent.TimeUnit

object AppConfig {
  val config = ConfigFactory.load
  val dictionaryPath = config.getString("dictionary.filename")
  val askTimeout =
    Timeout(config.getLong("akka.actors.askTimeoutInSeconds"), TimeUnit.SECONDS)
}

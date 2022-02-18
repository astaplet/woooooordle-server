package woooooordle.server.actors

import akka.actor.Actor
import akka.pattern.pipe
import com.github.blemale.scaffeine.{Cache, Scaffeine}
import woooooordle.server.objects.FieldColors.Green
import woooooordle.server.objects.{Dictionary, DictionaryLoader, FieldColor, GameDriver, TurnResult}

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

case class StartGameRequest(playerId: UUID, wordLength: Int)
case class StartGameResponse(playerId: UUID, wordLengthPossible: Boolean)

case class MakeGuessRequest(playerId: UUID, guess: String)
case class MakeGuessResponse(playerId: UUID, result: TurnResult)

class GameStateActor extends Actor with DictionaryLoader{
  val dictionaryFilePath: String = "" //TODO: Get this from configuration

  val cache: Cache[UUID, GameDriver] = Scaffeine()
    .expireAfterAccess(20.minutes)
    .build[UUID, GameDriver]

  val globalDictionary = loadDictionary(dictionaryFilePath)

  override def receive: Receive = {
    case msg: StartGameRequest => {
      var gameDictionary: Dictionary = Dictionary(Set.empty)
      try {
        gameDictionary = globalDictionary.getSingleLengthDictionary(msg.wordLength)
      } catch {
        case e: IllegalArgumentException => Future(StartGameResponse(msg.playerId, false)) pipeTo sender
      }
      cache.put(msg.playerId, GameDriver(gameDictionary))
      Future(StartGameResponse(msg.playerId, true)) pipeTo sender
    }

    case msg: MakeGuessRequest => {
      var gameDriver = cache.getIfPresent(msg.playerId)
        .map(driver => driver.takeTurn(msg.guess))
        .getOrElse(throw new IllegalArgumentException("Requested nonexistent game!"))
      if (gameDriver.previousTurnResult.validWord && gameDriver.previousTurnResult.coloredFields.forall(r => r._2 == Green)) {
        cache.invalidate(msg.playerId)
      }
      else if (gameDriver.turnsLeft <= 0) {
        cache.invalidate(msg.playerId)
      }
      else {
        cache.put(msg.playerId, gameDriver)
      }
      Future(MakeGuessResponse(msg.playerId, gameDriver.previousTurnResult)) pipeTo sender
    }
  }
}

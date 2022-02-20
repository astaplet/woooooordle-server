package woooooordle.server.cli

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import woooooordle.server.actors.{
  MakeGuessRequest,
  MakeGuessResponse,
  StartGameRequest,
  StartGameResponse
}
import woooooordle.server.objects.{AppConfig, FieldColors}

import java.util.UUID
import scala.concurrent.Await
import scala.io.StdIn._
import scala.concurrent.ExecutionContext.Implicits.global

/** This is a quick-and-dirty cli mostly just for my own in-dev testing and amusement. Not meant to be pretty
  */
object CLIClient {
  val playerId = UUID.randomUUID()

  def startGame(implicit gameStateActor: ActorRef): Unit = {

    implicit val timeout: Timeout = AppConfig.askTimeout
    print(Console.BLACK_B)
    var startGameResponse: StartGameResponse =
      StartGameResponse(playerId, false)
    var length: Int = -1
    while (!startGameResponse.wordLengthPossible) {
      while (length == -1) {
        print("Enter a word length to play: ")
        try {
          length = readInt()
        } catch {
          case e: NumberFormatException => println("Invalid input!")
        }
      }
      //

      Await.result(
        gameStateActor ? StartGameRequest(playerId, length) map (req => {
          startGameResponse = req.asInstanceOf[StartGameResponse]
          if (!startGameResponse.wordLengthPossible) {
            println(
              s"Not enough words of length ${length} in dictionary to start game :("
            )
          }
        }),
        timeout.duration
      )
    }
    takeTurn(length)
    System.exit(0)
  }

  def takeTurn(
      wordLength: Int
  )(implicit gameStateActor: ActorRef, timeout: Timeout): Unit = {
    var gameOver = false
    while (!gameOver) {
      var guess = ""
      while (guess.length != wordLength) {
        print("Make a guess: ")
        guess = readLine
        if (guess.length != wordLength) {
          println(s"Word must be of length $wordLength")
        }
      }
      val response = Await.result(
        gameStateActor ? MakeGuessRequest(playerId, guess) map (resp =>
          resp.asInstanceOf[MakeGuessResponse]
        ),
        timeout.duration
      )
      if (!response.result.validWord) {
        println("That word wasn't found in our dictionary. Make another guess!")
      } else {
        response.result.coloredFields.foreach(tup => {
          val output = tup._2 match {
            case FieldColors.Gray   => s"${tup._1.toString}".blackBg
            case FieldColors.Green  => s"${tup._1.toString}".greenBg
            case FieldColors.Yellow => s"${tup._1.toString}".yellowBg
          }
          print(output)
        })
        println(Console.BLACK_B)
        println(s"Guesses remaining ${response.result.turnsLeft}")
        if (response.result.turnsLeft == 0) {
          println(s"The word was: ${response.answer.getOrElse("")}")
          gameOver = true
        }
      }
    }
  }
  // with thanks to https://yobriefca.se/blog/2014/01/06/convenient-colouring-of-console-output-in-scala/
  implicit class ConsoleColorise(val str: String) extends AnyVal {

    import Console._

    def black = s"$BLACK$str"

    def green = s"$GREEN$str"

    def yellow = s"$YELLOW$str"

    def white = s"$WHITE$str"

    def blackBg = s"$BLACK_B$str"

    def greenBg = s"$GREEN_B$str"

    def yellowBg = s"$YELLOW_B$str"

    def whiteBg = s"$WHITE_B$str"

  }
}

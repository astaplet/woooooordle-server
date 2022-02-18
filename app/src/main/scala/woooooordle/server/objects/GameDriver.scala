package woooooordle.server.objects

import woooooordle.server.objects.FieldColors.{Gray, Green, Yellow}


sealed trait FieldColor {
  val name: String
}

object FieldColors {
  case object Gray extends FieldColor {
    val name = "Gray"
  }

  case object Green extends FieldColor {
    val name = "Green"
  }

  case object Yellow extends FieldColor {
    val name = "Yellow"
  }
}

case class TurnResult(coloredFields: Seq[(Char, FieldColor)], validWord: Boolean)

case class GameDriver(dictionary: Dictionary, word: String, turnsLeft: Int, previousTurnResult: TurnResult, wordMap: Map[Char, Set[Int]]) {
  def takeTurn(guessedWord: String): GameDriver = {
    if (!dictionary.wordExists(guessedWord)) {
      this.copy(previousTurnResult = previousTurnResult.copy(validWord = false))
    }
    else {
      val ca = guessedWord.toCharArray
      val letterCounts = scala.collection.mutable.Map.empty[Char, Int]
      val result: Seq[(Char, FieldColor)] = for {i <- 0 to ca.length} yield {
        if (wordMap.contains(ca(i))) {
          if (wordMap.get(ca(i)).get.contains(i)) {
            letterCounts.put(ca(i), letterCounts.getOrElse(ca(i), 0) + 1)
            (ca(i), Green)
          }
          else {
            if (wordMap.get(ca(i)).get.size > letterCounts.getOrElse(ca(i), 0)) {
              letterCounts.put(ca(i), letterCounts.getOrElse(ca(i), 0) + 1)
              (ca(i), Yellow)
            }
            else {
              (ca(i), Gray)
            }
          }
        }
        else {
          (ca(i), Gray)
        }
      }
      GameDriver(dictionary, word, turnsLeft - 1, TurnResult(result, true), wordMap)
    }
  }
}

object GameDriver {
  /**
   * Assumes the dictionary has already been set to a word length
   *
   * @param dictionary A dictionary containing words of a single length
   * @return An initialized game driver
   */
  def apply(dictionary: Dictionary): GameDriver = {
    val word = dictionary.getRandomWord
    GameDriver(dictionary, word)
  }
  private def apply(dictionary: Dictionary, word: String): GameDriver = {
    val ca = word.toCharArray
    val pairs = for {i <- 0 to ca.length} yield (ca(i), i)
    val wm: Map[Char, Set[Int]] = pairs.groupBy(_._1)
      .map(tup => (tup._1, tup._2.toSet.map(_._2)))
    GameDriver(dictionary, word, word.length, TurnResult(Seq.empty, true), wm)
  }
}

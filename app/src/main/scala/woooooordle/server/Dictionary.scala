package woooooordle.server

import scala.io.Source

case class Dictionary(entries: Set[String]) {
  def getMaxWordLength: Int = {
    this.entries.maxBy(_.length).length
  }

  def getSingleLengthDictionary(wordLength: Int): Dictionary = {
    val filteredEntries = this.entries.filter(_.length == wordLength)
    if (filteredEntries.isEmpty) {
      throw new IllegalArgumentException(s"No words of requested length $wordLength in this dictionary")
    }
    Dictionary(filteredEntries)
  }

  def getRandomWord: String = {
    val index = util.Random.nextInt(this.entries.size)
    entries.iterator.drop(index).next
  }

  def wordExists(word: String): Boolean = {
    this.entries.contains(word)
  }
}

trait DictionaryLoader {
  val dictEntries = Set.empty[String]

  def loadDictionary(filePath: String): Dictionary = {
    val source = Source.fromFile(filePath)
    val dict = Dictionary(source.getLines().toSet)
    source.close()
    dict
  }
}

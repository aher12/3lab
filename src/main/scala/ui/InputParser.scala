package ui

import io.Input
import scala.collection.immutable.LazyList

object InputParser {

  // Ленивый поток команд
  def commandStream: LazyList[String] = {
    Input.readWithPrompt("\nВведите команду (1-8): ")
  }

  // Парсинг команд 
  def parseCommand(stream: LazyList[String]): (Int, LazyList[String]) = {
    val (numOpt, rest) = io.Input.readInt(stream)

    if (numOpt.isEmpty) {
      println("Ожидается число от 1 до 9")
      return parseCommand(rest)
    }

    val num = numOpt.get

    if (num < 1 || num > 9) {
      println(s"Команда должна быть 1-9, получено: $num")
      return parseCommand(rest)
    }

    (num, rest)
  }
}
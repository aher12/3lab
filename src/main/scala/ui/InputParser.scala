package ui

import io.Input
import scala.collection.immutable.LazyList

object InputParser {

  // Ленивый поток команд
  def commandStream: LazyList[String] = {
    Input.readWithPrompt("")
  }
}
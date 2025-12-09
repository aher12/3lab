package io

import scala.collection.immutable.LazyList
import scala.io.StdIn
import scala.util.Try

object Input {

  // Рекурсивный поток ввода 
  lazy val stream: LazyList[String] = {
    Option(StdIn.readLine()).map(_ #:: stream).getOrElse(LazyList.empty)
  }

  type Result[T] = (T, LazyList[String])

  // Чтение с подсказкой - ТОЛЬКО если нужно
  def readWithPrompt(prompt: String): LazyList[String] = {
    if (prompt.nonEmpty) print(prompt)
    stream
  }

  // Чтение числа
  def readInt(stream: LazyList[String]): (Option[Int], LazyList[String]) = {
    if (stream.isEmpty) {
      return (None, stream)
    }

    val line #:: rest = stream

    Try(line.toInt).toOption match {
      case Some(num) => (Some(num), rest)
      case None => (None, rest)
    }
  }
}
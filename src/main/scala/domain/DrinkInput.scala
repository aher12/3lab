package domain

import scala.collection.immutable.LazyList
import scala.util.Try

object DrinkInput {

  // Главный метод: читаем напиток из потока
  def readDrink(in: LazyList[String]): (Option[Drink], LazyList[String]) = {
    // Шаг 1: Читаем тип напитка
    val (typeNumOpt, rest1) = readValidNumber(in, 1, 4, "тип напитка")

    if (typeNumOpt.isEmpty) {
      return (None, rest1)
    }

    val drinkType = DrinkType.all(typeNumOpt.get - 1)

    // Шаг 2: Читаем бренд
    val (brandNumOpt, rest2) = readValidNumber(rest1, 1, 3, "бренд")

    if (brandNumOpt.isEmpty) {
      return (None, rest2)
    }

    val brand = Brand.all(brandNumOpt.get - 1)

    // Шаг 3: Читаем сахар
    val (sugarOpt, rest3) = readValidNumber(rest2, 0, 10, "количество сахара")

    if (sugarOpt.isEmpty) {
      return (None, rest3)
    }

    // Шаг 4: Создаём напиток
    val drink = Drink.create(drinkType, brand, sugarOpt.get)
    (Some(drink), rest3)
  }

  // Вспомогательный метод: читает валидное число
  private def readValidNumber(
                               stream: LazyList[String],
                               min: Int,
                               max: Int,
                               fieldName: String
                             ): (Option[Int], LazyList[String]) = {

    // Пропускаем не-числа
    val validStream = stream.dropWhile(s => Try(s.toInt).isFailure)

    if (validStream.isEmpty) {
      println(s"Ожидается число ($fieldName)")
      (None, if (stream.nonEmpty) stream.tail else stream)
    } else {
      val numStr #:: rest = validStream

      Try(numStr.toInt).toOption match {
        case Some(num) if num >= min && num <= max =>
          (Some(num), rest)
        case Some(num) =>
          println(s"$fieldName должно быть от $min до $max")
          (None, rest)
        case None =>
          println(s"Неверный формат числа ($fieldName)")
          (None, rest)
      }
    }
  }
}
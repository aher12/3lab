package util

import scala.collection.immutable.LazyList

object LazyListUtils {

  // 1. Ленивое чтение пользовательского ввода
  def readUserInput(prompt: String = "> "): LazyList[String] = {
    LazyList.continually {
      print(prompt)
      scala.io.StdIn.readLine().trim
    }
  }

  // 2. Ленивая генерация меню
  def generateAllDrinks: LazyList[(String, String, Int)] = {
    val drinkTypes = List("Черный чай", "Зеленый чай", "Черный кофе", "Кофе с молоком")
    val brands = List("Обычный", "Премиум", "Люкс")

    LazyList.from(
      for {
        drink <- drinkTypes
        brand <- brands
        sugar <- 0 to 5
      } yield (drink, brand, sugar)
    )
  }

  // 3. Ленивый фильтр по типу напитка
  def filterDrinksByType(drinkType: String): LazyList[(String, String, Int)] = {
    generateAllDrinks.filter(_._1 == drinkType)
  }

  // 4. Ленивый расчет цен
  def calculatePrices(drinks: LazyList[(String, String, Int)]): LazyList[Double] = {
    drinks.map { case (drinkType, brand, sugar) =>
      val basePrice = drinkType match {
        case "Черный чай" => 40.0
        case "Зеленый чай" => 50.0
        case "Черный кофе" => 70.0
        case "Кофе с молоком" => 80.0
      }
      val multiplier = brand match {
        case "Обычный" => 1.0
        case "Премиум" => 1.5
        case "Люкс" => 2.0
      }
      (basePrice * multiplier) + (sugar * 5.0)
    }
  }

  // 5. Пакетная обработка (ленивые батчи)
  def batchProcess[A](stream: LazyList[A], batchSize: Int): LazyList[List[A]] = {
    if (stream.isEmpty) LazyList.empty
    else {
      val (batch, rest) = stream.splitAt(batchSize)
      batch.toList #:: batchProcess(rest, batchSize)
    }
  }

  // 6. Ленивое отображение прогресса
  def showProgress[A](stream: LazyList[A], label: String): LazyList[A] = {
    stream.zipWithIndex.map { case (item, index) =>
      if (index % 10 == 0) print(s"\r$label: обработано $index")
      item
    }
  }
}
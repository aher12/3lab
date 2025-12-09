package service

import domain.{Drink, Menu}

object PriceCalculator {

  def calculateTotal(menu: Menu): Double = menu.totalPrice

  def calculateAverage(menu: Menu): Option[Double] = {
    if (menu.isEmpty) {
      return None
    }
    Some(menu.totalPrice / menu.size)
  }

  def findMostExpensive(menu: Menu): Option[Drink] = {
    if (menu.isEmpty) {
      return None
    }
    Some(menu.drinks.maxBy(_.totalPrice))
  }

  def findCheapest(menu: Menu): Option[Drink] = {
    if (menu.isEmpty) {
      return None
    }
    Some(menu.drinks.minBy(_.totalPrice))
  }

  def priceStatistics(menu: Menu): Map[String, Any] = {
    val total = calculateTotal(menu)
    val average = calculateAverage(menu).getOrElse(0.0)
    val count = menu.size
    val mostExpensive = findMostExpensive(menu).map(_.description).getOrElse("Нет напитков")
    val cheapest = findCheapest(menu).map(_.description).getOrElse("Нет напитков")

    Map(
      "total" -> total,
      "average" -> average,
      "count" -> count,
      "mostExpensive" -> mostExpensive,
      "cheapest" -> cheapest
    )
  }
}
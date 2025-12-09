package service

import domain.{Menu, Drink, DrinkType, Brand}
import scala.collection.immutable.LazyList

object MenuService {

  def createMenuFromConfigs(configs: List[(DrinkType, Brand, Int)]): Menu = {
    val drinks = configs.map { case (dt, b, s) => Drink.create(dt, b, s) }
    Menu(drinks)
  }

  def filterMenuByType(menu: Menu, drinkType: DrinkType): Menu = {
    Menu(menu.drinks.filter(_.drinkType == drinkType))
  }

  def filterMenuByBrand(menu: Menu, brand: Brand): Menu = {
    Menu(menu.drinks.filter(_.brand == brand))
  }

  def sortByPrice(menu: Menu, ascending: Boolean = true): Menu = {
    val sortedDrinks = if (ascending) {
      menu.drinks.sortBy(_.totalPrice)
    } else {
      menu.drinks.sortBy(d => -d.totalPrice)
    }
    Menu(sortedDrinks)
  }
}
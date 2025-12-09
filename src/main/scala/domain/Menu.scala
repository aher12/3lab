package domain

final case class Menu(drinks: List[Drink]) {
  def addDrink(drink: Drink): Menu = Menu(drink :: drinks)

  def removeDrink(index: Int): Option[Menu] = {
    if (index >= 0 && index < drinks.length) {
      Some(Menu(drinks.patch(index, Nil, 1)))
    } else {
      None
    }
  }

  def totalPrice: Double = drinks.map(_.totalPrice).sum

  def isEmpty: Boolean = drinks.isEmpty

  def size: Int = drinks.length
}

object Menu {
  val empty: Menu = Menu(List.empty)
}
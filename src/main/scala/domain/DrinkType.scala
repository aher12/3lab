package domain

sealed trait DrinkType
object DrinkType {
  case object BlackTea extends DrinkType
  case object GreenTea extends DrinkType
  case object BlackCoffee extends DrinkType
  case object CoffeeWithMilk extends DrinkType

  val all: List[DrinkType] = List(BlackTea, GreenTea, BlackCoffee, CoffeeWithMilk)

  def fromString(s: String): Option[DrinkType] = s.toLowerCase match {
    case "черный чай" | "black tea" => Some(BlackTea)
    case "зеленый чай" | "green tea" => Some(GreenTea)
    case "черный кофе" | "black coffee" => Some(BlackCoffee)
    case "кофе с молоком" | "coffee with milk" => Some(CoffeeWithMilk)
    case _ => None
  }

  def displayName(drinkType: DrinkType): String = drinkType match {
    case BlackTea => "Черный чай"
    case GreenTea => "Зеленый чай"
    case BlackCoffee => "Черный кофе"
    case CoffeeWithMilk => "Кофе с молоком"
  }

  def basePrice(drinkType: DrinkType): Double = drinkType match {
    case BlackTea => 40.0
    case GreenTea => 50.0
    case BlackCoffee => 70.0
    case CoffeeWithMilk => 80.0
  }
}
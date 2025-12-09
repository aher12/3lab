package domain

final case class Drink(
                        drinkType: DrinkType,
                        brand: Brand,
                        sugarSpoons: Int,
                        basePrice: Double
                      ) {
  lazy val totalPrice: Double = {
    val brandMultiplier = Brand.priceMultiplier(brand)
    val sugarPrice = sugarSpoons * 5.0
    (basePrice * brandMultiplier) + sugarPrice
  }

  def description: String = {
    val typeName = DrinkType.displayName(drinkType)
    val brandName = Brand.displayName(brand)
    val sugarInfo = if (sugarSpoons > 0) s" с $sugarSpoons ложками сахара" else ""
    s"$typeName ($brandName)$sugarInfo"
  }
}

object Drink {
  def create(drinkType: DrinkType, brand: Brand, sugar: Int): Drink = {
    val basePrice = drinkType match {
      case DrinkType.BlackTea => 40.0
      case DrinkType.GreenTea => 50.0
      case DrinkType.BlackCoffee => 70.0
      case DrinkType.CoffeeWithMilk => 80.0
    }
    Drink(drinkType, brand, sugar, basePrice)
  }
}
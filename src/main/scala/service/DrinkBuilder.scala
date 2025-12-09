package service

import domain.{Drink, DrinkType, Brand}
import scala.collection.immutable.LazyList

object DrinkBuilder {

  type DrinkConfig = (DrinkType, Brand, Int)

  val defaultConfigs: LazyList[DrinkConfig] = LazyList.from(
    for {
      drinkType <- DrinkType.all
      brand <- Brand.all
      sugar <- 0 to 3
    } yield (drinkType, brand, sugar)
  )

  def buildFromConfig(config: DrinkConfig): Drink = {
    val (drinkType, brand, sugar) = config
    Drink.create(drinkType, brand, sugar)
  }

  def parseConfig(input: String): Option[DrinkConfig] = {
    val parts = input.split(",").map(_.trim)
    if (parts.length == 3) {
      for {
        drinkType <- DrinkType.fromString(parts(0))
        brand <- Brand.fromString(parts(1))
        sugar <- parts(2).toIntOption if sugar >= 0 && sugar <= 10
      } yield (drinkType, brand, sugar)
    } else {
      None
    }
  }
}
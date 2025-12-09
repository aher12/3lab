package domain

sealed trait Brand
object Brand {
  case object Default extends Brand
  case object Premium extends Brand
  case object Luxury extends Brand

  val all: List[Brand] = List(Default, Premium, Luxury)

  def fromString(s: String): Option[Brand] = s.toLowerCase match {
    case "обычный" | "default" => Some(Default)
    case "премиум" | "premium" => Some(Premium)
    case "люкс" | "luxury" => Some(Luxury)
    case _ => None
  }

  def displayName(brand: Brand): String = brand match {
    case Default => "Обычный"
    case Premium => "Премиум"
    case Luxury => "Люкс"
  }

  def priceMultiplier(brand: Brand): Double = brand match {
    case Default => 1.0
    case Premium => 1.5
    case Luxury => 2.0
  }
}
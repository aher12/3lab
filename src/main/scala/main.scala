import domain.{Menu, Drink, DrinkType, Brand}
import ui.{ConsoleUI, InputParser}
import io.Input
import scala.collection.immutable.LazyList

object Main extends App {

  // Тестовое меню из 10 напитков
  def createTestMenu(): Menu = {
    val testDrinks = List(
      Drink.create(DrinkType.BlackTea, Brand.Default, 0),      // 40 руб
      Drink.create(DrinkType.BlackTea, Brand.Default, 2),      // 50 руб
      Drink.create(DrinkType.GreenTea, Brand.Default, 1),      // 55 руб
      Drink.create(DrinkType.BlackTea, Brand.Premium, 0),      // 60 руб
      Drink.create(DrinkType.GreenTea, Brand.Premium, 1),      // 80 руб
      Drink.create(DrinkType.CoffeeWithMilk, Brand.Default, 0), // 80 руб
      Drink.create(DrinkType.BlackCoffee, Brand.Default, 3),   // 85 руб
      Drink.create(DrinkType.BlackCoffee, Brand.Premium, 1),   // 110 руб
      Drink.create(DrinkType.CoffeeWithMilk, Brand.Premium, 2), // 130 руб
      Drink.create(DrinkType.BlackCoffee, Brand.Luxury, 0)     // 140 руб
    )
    Menu(testDrinks)
  }

  ConsoleUI.displayWelcome()

  // Начинаем с тестового меню
  val initialMenu = createTestMenu()
  println("\n" + "=" * 60)
  println("ЗАГРУЖЕНО ТЕСТОВОЕ МЕНЮ")
  println("=" * 60)
  println(s"• ${initialMenu.size} напитков")
  println(f"• Общая стоимость: ${initialMenu.totalPrice}%.2f руб.")
  println("• Все типы напитков и бренды")
  println("=" * 60)

  // Показываем начальное меню
  ConsoleUI.displayMenu(initialMenu)
  ConsoleUI.showCommands()

  // Основной цикл
  def runWithLazyStream(startMenu: Menu): Menu = {
    // Создаем поток ввода ВРУЧНУЮ
    def inputStream: LazyList[String] = {
      print("\nВведите команду (1-8): ")
      scala.io.StdIn.readLine().trim #:: inputStream
    }

    @annotation.tailrec
    def loop(menu: Menu, stream: LazyList[String]): Menu = {
      if (stream.isEmpty) return menu

      val cmd #:: rest = stream

      cmd match {
        case "8" =>
          println("\nЗавершение работы...")
          menu

        case cmd if "1234567".contains(cmd) =>
          val newMenu = ConsoleUI.processChoice(cmd, menu)
          loop(newMenu, inputStream) // Берем НОВЫЙ поток!

        case _ =>
          println("Неверная команда. Введите число от 1 до 8")
          loop(menu, inputStream) // Берем НОВЫЙ поток!
      }
    }

    loop(startMenu, inputStream)
  }

  // Выбирай что хочешь:
  // val finalMenu = runWithSimpleLoop(initialMenu)      // Простой вариант
  val finalMenu = runWithLazyStream(initialMenu) // С LazyList

  // Финальный вывод
  println("\n" + "=" * 60)
  println("ФИНАЛЬНОЕ МЕНЮ")
  println("=" * 60)
  ConsoleUI.displayMenu(finalMenu)
  ConsoleUI.displayStatistics(finalMenu)

  println("\n" + "=" * 60)
  println("СРАВНЕНИЕ С НАЧАЛОМ РАБОТЫ")
  println("=" * 60)
  println(f"Начало: ${initialMenu.size} напитков, ${initialMenu.totalPrice}%.2f руб.")
  println(f"Конец:  ${finalMenu.size} напитков, ${finalMenu.totalPrice}%.2f руб.")

  val difference = finalMenu.totalPrice - initialMenu.totalPrice
  if (difference > 0) {
    println(f"Изменение: +${difference}%.2f руб.")
  } else if (difference < 0) {
    println(f"Изменение: ${difference}%.2f руб.")
  } else {
    println("Изменений нет")
  }

  println("\nСпасибо за использование программы!")
}
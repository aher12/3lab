package ui

import domain.{Brand, Drink, DrinkType, Menu, DrinkInput}
import service.{MenuService, PriceCalculator}
import io.Input
import scala.collection.immutable.LazyList
import scala.util.Try

object ConsoleUI {

  def displayMenu(menu: Menu): Unit = {
    if (menu.isEmpty) {
      println("\nМеню пустое")
    } else {
      println("\n" + "=" * 60)
      println("ТЕКУЩЕЕ МЕНЮ")
      println("=" * 60)
      menu.drinks.zipWithIndex.foreach { case (drink, index) =>
        println(f"${index + 1}%3d. ${drink.description}%-50s ${drink.totalPrice}%6.2f руб.")
      }
      println("-" * 60)
      println(f"ИТОГО: ${menu.totalPrice}%58.2f руб.")
      println(s"Напитков: ${menu.size}")
      println("=" * 60)
    }
  }

  def displayStatistics(menu: Menu): Unit = {
    val stats = PriceCalculator.priceStatistics(menu)
    println("\n" + "=" * 60)
    println("СТАТИСТИКА МЕНЮ")
    println("=" * 60)
    println(f"Общая стоимость: ${stats("total").asInstanceOf[Double]}%45.2f руб.")
    println(f"Средняя цена: ${stats("average").asInstanceOf[Double]}%48.2f руб.")
    println(f"Количество напитков: ${stats("count").asInstanceOf[Int]}%44d")
    println(s"Самый дорогой: ${stats("mostExpensive")}")
    println(s"Самый дешевый: ${stats("cheapest")}")
    println("=" * 60)
  }

  def showCommands(): Unit = {
    println("\n" + "=" * 50)
    println("КОМАНДЫ:")
    println("=" * 50)
    println("1. Добавить напиток")
    println("2. Удалить напиток")
    println("3. Показать текущее меню")
    println("4. Показать статистику")
    println("5. Фильтровать по типу напитка (просмотр)")
    println("6. Фильтровать по бренду (просмотр)")
    println("7. Сортировать меню по цене")
    println("8. Выйти из программы")
    println("=" * 50)
  }

  def displayWelcome(): Unit = {
    println("=" * 60)
    println("КАФЕ 'ФУНКЦИОНАЛЬНЫЙ СТИЛЬ'")
    println("=" * 60)

    println("\nТИПЫ НАПИТКОВ:")
    DrinkType.all.foreach { dt =>
      val price = DrinkType.basePrice(dt)
      println(f"  ${DrinkType.displayName(dt)}%-20s - ${price}%.1f руб.")
    }

    println("\nБРЕНДЫ:")
    Brand.all.foreach { b =>
      val multiplier = Brand.priceMultiplier(b)
      println(f"  ${Brand.displayName(b)}%-10s - множитель ${multiplier}%.1f")
    }

    println("\nЦена сахара: 5 руб. за ложку")
  }

  // Вспомогательные методы

  private def showDrinkTypes(): Unit = {
    println("Типы напитков:")
    DrinkType.all.zipWithIndex.foreach { case (dt, idx) =>
      println(s"${idx + 1}. ${DrinkType.displayName(dt)}")
    }
  }

  private def showBrands(): Unit = {
    println("\nБренды:")
    Brand.all.zipWithIndex.foreach { case (brand, idx) =>
      println(s"${idx + 1}. ${Brand.displayName(brand)}")
    }
  }

  // Обработчики команд

  private def handleAddDrink(currentMenu: Menu): Menu = {
    println("\n" + "=" * 40)
    println("ДОБАВЛЕНИЕ НАПИТКА")
    println("=" * 40)

    // Показываем типы напитков
    showDrinkTypes()

    // Получаем поток ввода
    val typeStream = Input.readWithPrompt("Введите номер типа (1-4): ")
    val (typeNumOpt, _) = Input.readInt(typeStream)

    if (typeNumOpt.isEmpty) {
      println("Неверный тип напитка")
      return currentMenu
    }

    val drinkType = DrinkType.all(typeNumOpt.get - 1)

    // Показываем бренды
    showBrands()

    val brandStream = Input.readWithPrompt("Введите номер бренда (1-3): ")
    val (brandNumOpt, _) = Input.readInt(brandStream)

    if (brandNumOpt.isEmpty) {
      println("Неверный бренд")
      return currentMenu
    }

    val brand = Brand.all(brandNumOpt.get - 1)

    // Читаем сахар
    val sugarStream = Input.readWithPrompt("Количество ложек сахара (0-10): ")
    val (sugarOpt, _) = Input.readInt(sugarStream)

    if (sugarOpt.isEmpty) {
      println("Неверное количество сахара")
      return currentMenu
    }

    val sugar = sugarOpt.get

    if (sugar < 0 || sugar > 10) {
      println("Количество сахара должно быть от 0 до 10")
      return currentMenu
    }

    // Создаём напиток
    val drink = Drink.create(drinkType, brand, sugar)
    println(s"\nДобавлен: ${drink.description}")
    println(f"Цена: ${drink.totalPrice}%.2f руб.")

    currentMenu.addDrink(drink)
  }

  private def handleRemoveDrink(currentMenu: Menu): Menu = {
    if (currentMenu.isEmpty) {
      println("\nМеню пустое")
      return currentMenu
    }

    // НЕ показываем меню здесь!
    print("\nВведите номер напитка для удаления: ")
    val input = scala.io.StdIn.readLine()

    scala.util.Try(input.toInt).toOption match {
      case Some(index) if index >= 1 && index <= currentMenu.size =>
        val newDrinks = currentMenu.drinks.patch(index - 1, Nil, 1)
        println(s"Напиток №$index удален")
        Menu(newDrinks)

      case Some(index) =>
        println(s"Номер должен быть от 1 до ${currentMenu.size}")
        currentMenu

      case None =>
        println("Неверный номер")
        currentMenu
    }
  }
  private def handleFilterByType(currentMenu: Menu): Menu = {
    println("\n" + "=" * 40)
    println("ФИЛЬТРАЦИЯ ПО ТИПУ (только просмотр)")
    println("=" * 40)

    showDrinkTypes()

    val inputStream = Input.readWithPrompt("Введите номер типа (1-4): ")
    val (numOpt, _) = Input.readInt(inputStream)

    if (numOpt.isEmpty) {
      println("Неверный тип напитка")
      return currentMenu
    }

    val typeNum = numOpt.get

    if (typeNum < 1 || typeNum > 4) {
      println("Тип должен быть от 1 до 4")
      return currentMenu
    }

    val drinkType = DrinkType.all(typeNum - 1)
    val filtered = MenuService.filterMenuByType(currentMenu, drinkType)

    println(s"\nНапитки типа '${DrinkType.displayName(drinkType)}':")
    displayMenu(filtered)

    currentMenu
  }

  private def handleFilterByBrand(currentMenu: Menu): Menu = {
    println("\n" + "=" * 40)
    println("ФИЛЬТРАЦИЯ ПО БРЕНДУ (только просмотр)")
    println("=" * 40)

    showBrands()

    val inputStream = Input.readWithPrompt("Введите номер бренда (1-3): ")
    val (numOpt, _) = Input.readInt(inputStream)

    if (numOpt.isEmpty) {
      println("Неверный бренд")
      return currentMenu
    }

    val brandNum = numOpt.get

    if (brandNum < 1 || brandNum > 3) {
      println("Бренд должен быть от 1 до 3")
      return currentMenu
    }

    val brand = Brand.all(brandNum - 1)
    val filtered = MenuService.filterMenuByBrand(currentMenu, brand)

    println(s"\nНапитки бренда '${Brand.displayName(brand)}':")
    displayMenu(filtered)

    currentMenu
  }

  private def handleSortMenu(currentMenu: Menu): Menu = {
    println("\n" + "=" * 40)
    println("СОРТИРОВКА МЕНЮ")
    println("=" * 40)
    println("1. По возрастанию цены (от дешевых к дорогим)")
    println("2. По убыванию цены (от дорогих к дешевым)")

    val inputStream = Input.readWithPrompt("Введите номер (1-2): ")
    val (numOpt, _) = Input.readInt(inputStream)

    if (numOpt.isEmpty) {
      println("Неверный выбор")
      return currentMenu
    }

    val choice = numOpt.get

    if (choice == 1) {
      val sorted = MenuService.sortByPrice(currentMenu, ascending = true)
      println("\nМеню отсортировано по возрастанию цены")
      sorted
    } else if (choice == 2) {
      val sorted = MenuService.sortByPrice(currentMenu, ascending = false)
      println("\nМеню отсортировано по убыванию цены")
      sorted
    } else {
      println("Неверный выбор")
      currentMenu
    }
  }

  // Главный метод обработки

  def processChoice(choice: String, currentMenu: Menu): Menu = {
    val newMenu = choice match {
      case "1" => handleAddDrink(currentMenu)
      case "2" => handleRemoveDrink(currentMenu)
      case "3" =>
        displayMenu(currentMenu)
        currentMenu
      case "4" =>
        displayStatistics(currentMenu)
        currentMenu
      case "5" => handleFilterByType(currentMenu)
      case "6" => handleFilterByBrand(currentMenu)
      case "7" => handleSortMenu(currentMenu)
      case "8" =>
        println("\nЗавершение работы...")
        currentMenu
      case _ =>
        println("\nНеверная команда. Введите число от 1 до 8")
        currentMenu
    }

    // Показываем команды снова (кроме выхода)
    if (choice != "8") {
      showCommands()
    }

    newMenu
  }
}
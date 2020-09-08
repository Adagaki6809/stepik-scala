package stepik

import scala.io.StdIn.{readInt, readLine}
import scala.math.BigInt.int2bigInt

object Main extends App {

  // 2.4 Пространства имен
  import Config.{name => prefix}
  def greeting(name: String) {
    println(prefix + name)
  }
  val name = "Oleg"
  greeting(name)

  // 3.1 Типы
  val x: Double = 3.4
  import scala.math.{pow, E, Pi, sqrt}
  def normalDistribution(mu: Double, sigma: Double, x: Double): Double = {
    (1/(sigma*sqrt(2*Pi))) * pow(E, -((x-mu)*(x-mu))/(2*sigma*sigma))
  }

  // 3.2 Числа
  // Задача 1
  import scala.math.BigDecimal.RoundingMode.HALF_UP
  def crispsWeight(weight: BigDecimal, potatoWaterRatio: Double, crispsWaterRatio: Double): BigDecimal = {
    ((weight - potatoWaterRatio*weight)/(1 - crispsWaterRatio)).setScale(5,HALF_UP)
  }
  println(crispsWeight(90.0,0.9,0.1))

  // Задача 2
  println(readLine.toInt.toBinaryString.toCharArray.count(_ == '1'))

  // 3.3 Булевые значения
  def isCapital(word: String, pos: Int): Boolean = {
    // your code here
    val code = word.charAt(pos).toInt
    code > 64 && code < 91
  }
  isCapital("Scala",0)

  // 3.4 Строки
  // Задача 1
  val Array(startIndex, endIndex) = readLine.split(" ").map(_.toInt)
  val s = readLine()
  println(s.substring(0,startIndex) + s.substring(startIndex, endIndex+1).reverse + s.substring(endIndex+1))

  // Задача 2
  val regex = "^[a-z]([a-z]+_?[a-z]+)*[a-z]?$".r
  val s2 = "snake_case"

  regex.findFirstMatchIn(s2) match {
    case Some(_) => println(true)
    case None => println(false)
  }

  // 4.1 Методы
  // Задача 1
  def fibs(num: Int): Int = {
    if (num == 1) 1
    else if (num == 2) 1
      else fibs(num-1) + fibs(num-2)
  }
  println(fibs(3))

  // Задача 2
  val getGift = () => {
    println("The gift is received")
    readLine.toInt
  }
  def sendGift(currentAmount: Int, gift: => Int) = {
    if (currentAmount >= 500)
      currentAmount + gift
    else
      currentAmount
  }
  val accountAmounts = List(100, 200, 500, 300, 700)
  val newAmounts = accountAmounts.map(amount => sendGift(amount, getGift()))
  println(newAmounts)

  // 4.2 Функции
  object LessonData{
    def searchInArray(cond: Int => Boolean, array: List[Int]): List[Int] = {
        array.filter(cond)
    }
  }
  val searchOdd = LessonData.searchInArray(_ % 2 == 1, _: List[Int])
  println(searchOdd(List(8,11,12))) // List(11)

  // 4.3 Обобщенные методы
  // Задача 1
  import scala.annotation.tailrec
  @tailrec
  def fibs(n: Int, currentNumber: Int = 1, f1: BigInt  = 0, f2: BigInt  = 1): BigInt  = {
    if (n == currentNumber)
        f2
    else {
        fibs(n-1,currentNumber,f2,f1+f2)
    }
  }
  println(fibs(10))

  // Задача 2
  def middle[T](data: Iterable[T]): T = {
    data.splitAt(data.size / 2)._2.head
  }
  require(middle("Scala") == 'a')
  require(middle(Seq(1, 7, 5, 9)) == 5)

  // 5.1 Управляющие конструкции
  val n = readInt()
  for {
    i <- 1 until n
    j <- 1 until n if ((BigInt(i) gcd BigInt(j)) == 1)
  } println(s"$i  $j")

  // 5.2 Pattern matching
  // Задача 1
  case class Pet(name: String, says: String)
  val pet = Pet("Tom", "nyameow")
  val kind = pet match {
    case Pet(_, says) if "(meow|nya)".r.findFirstMatchIn(says).isDefined => "cat"
    case Pet(name, _) if "(Rex)".r.findFirstMatchIn(name).isDefined => "dog"
    case Pet(_, says) if "(0|1)".r.findFirstMatchIn(says).isDefined => "robot"
    case Pet(_, _) => "unknown"
  }
  println(kind)

  // Задача 2
  val nameRegex = "([a-zA-Z]+)".r
  val emailRegex = "\\w+@(\\w+\\.\\w+)".r
  val nameAndEmailRegex = "([a-zA-Z]+) \\w+@(\\w+\\.\\w+)".r
  val input = List("oleg oleg@email.com","7bdaf0a1be3","a8af118b1a2","28d74b7a3fe")
  val result = input match {
    case nameRegex(name):: emailRegex(domain) :: _ => s"$name $domain"
    case nameAndEmailRegex(name, domain) :: _ => s"$name $domain"
    case _ => "invalid"
  }
  println(result)

  // 5.3 Partial functions
  // Задача 1
  val log: PartialFunction[Double, Double] = {
    case x if x > 0 => Math.log(x)
    case _ => Double.NaN
  }
  println(log(-1))

  // Задача 2
  case class Jar(name: String, value: Int, price: Double)
  val jars = List(Jar("Морской синий 6л", 6, 3000), Jar("Огненно-красный 12л", 12, 5000))
  def discount: PartialFunction[Jar, String] = {
    case Jar(name, value, price) if value > 10 => s"$name ${price*0.1}"
    case Jar(name, value, price) if value >= 5 => s"$name ${price*0.05}"
  }
  println(jars.collect(discount))

  // 5.4 Кортежи
  def swap3(tuple: (Int, Int, Int)): (Int, Int, Int) = {
    (tuple._3, tuple._2, tuple._1)
  }
  println(swap3((1,2,3)))

  // 5.5 Опциональные значения
  def foo(list: List[Int]): Int = list.find(_ % 3 == 0).map(_ * 2).getOrElse(0)
  println(foo(List(1, 2, 3, 4, 5, 6)))

  // 5.6 Either
  // Задача 1
  def divide(p: (Int, Int))(q: (Int, Int)): Either[String, (Int, Int)] = {
    if (p._2 == 0 || q._2 == 0 || q._1 == 0)
      Left("Zero divisor")
    else if ((Math.abs(p._1) >= Math.abs(p._2)) || (Math.abs(q._1) >= Math.abs(q._2)))
      Left("Invalid input")
    else if (Math.abs(p._1 * q._2) >= Math.abs(p._2 * q._1))
      Left("Improper result")
    else Right((((p._1 * q._2)/(BigInt(p._1 * q._2) gcd BigInt(p._2 * q._1))).toInt,((p._2 * q._1)/(BigInt(p._1 * q._2) gcd BigInt(p._2 * q._1))).toInt))
  }
  println(divide(1,10)(2,10))

  // Задача 2
  def foo(options: List[Option[Int]]): List[Int] = options.collect(notNone)
  val notNone: PartialFunction[Option[Int], Int] = {
    case Some(x) => x
  }
  println(foo(List(Option(1),Option(2), None)))

  // 6.1 Коллекции. Часть 1.
  val ints: List[Int] = List(1,0,1,0,1,0,1,0,1,1,0,1)
  println(ints.filter(_==0).mkString(", "))
  println(ints.filter(_==1).mkString(", "))

/*  def partition(list: List[Int], left: Int, right: Int): Int = left match {
    case left if left >= 0  && right!=list.length =>
      list.splitAt(left)._2.splitAt(right-left)._1.splitAt(list.splitAt(left)._2.splitAt(right-left)._1.length/2)._2.head
    case _  => list.head
  }
  val k = 4
  val list = "3 8 1 12 23".split(" ").toList.map(_.toInt)
  println(partition(list,0,4))*/


  // 6.2 Коллекции. Часть 2.
  // Задача 1
  val list2 = List(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150)
  list2.takeWhile(_ < 100).filter(_ % 4 == 0).map(_ / 4).init.foreach(println)

  // Задача 2
  println(Stream.continually(readLine()).takeWhile(_!="END").zipWithIndex.filter(x=>(x._2) % 2 > 0).map(x => x._1.toInt * 2).sum)

  // Задача 3
  val points: List[Int] = List(1,3) // точки кроссинговера
  val chr1: List[Char] = List('x','x','x','x','x') // первая хромосома
  val chr2: List[Char] = List('y','y','y','y','y') // вторая хромосома
  def rec(points: List[Int], chr1: List[Char], chr2: List[Char]): String = {
    if (points != Nil) {
    rec(points.tail,chr1.splitAt(points.head)._1 ++ chr2.splitAt(points.head)._2,
    chr2.splitAt(points.head)._1 ++ chr1.splitAt(points.head)._2)
  }
  else 
    (List[String]() :+ chr1.mkString("") :+ chr2.mkString("")).mkString("\n")
  }
  println(rec(points,chr1,chr2))

  // Задача 5
  abstract class DiffList[A](calculate: List[A] => List[A]) {
    def prepend(s: List[A]): DiffList[A]
    def append(s: List[A]): DiffList[A]
    def result: List[A]
  }
  final class DiffListImpl[A](listFunc: List[A] => List[A]) extends DiffList[A](listFunc) {
    def prepend(s: List[A]):DiffListImpl[A] =  new DiffListImpl[A](s ++ listFunc(_))
    def append(s: List[A]):DiffListImpl[A] =  new DiffListImpl[A](listFunc(_) ++ s)
    def result: List[A] = listFunc(Nil)
  }
  val l1 = List(1,2,3)
  val l2 = List(4,5,6)
  val f = identity(List[Int]_)
  val dl = new DiffListImpl[Int](f)
  val result2 = dl.append(l2).prepend(l1).result // List(1,2,3,4,5,6)
  println(result2)

  // 6.3 For comprehension
  // Задача 1
  val list1 = List(1, 3, 5, 7)
  val list22 = List(2, 4, 6, 8)
  val list3 = List(1, 3, 5, 8, 10, 12, 14)
  for {
    x <- list1
    y <- list22
    z <- list3 if x * y == z
  }  println((x,y))

  // Задача 2
  def service1: Either[String, Double] = Right(1.0)
  def service2(res1: Double): Either[String, Int] = Right(22)
  def service3: String ="333"
  def service4(res1: Double, res2: Int, res3: String): Either[String, String] = Right(res1.toString+res2.toString+res3)
  def res1 = for {
    r1 <- service1
    r2 <- service2(r1)
    res <- service4(r1,r2,service3)
  } yield res
  println(res1)

  // 7.1 Классы
  class Waiter(name: String, order: List[String]) {
    if (order==Nil) println(s"My name $name")
    def giveMe(s: String):Waiter = {
      new Waiter(name, s +: order)
    }
    def complete(): List[String] = order.reverse
  }
  val waiter = new Waiter("иван",List())
  val positions = waiter.giveMe("борщ").giveMe("хлеб").complete()
  println(s"Order: ${positions.mkString(",")}")

  // 7.2 Абстрактные классы
  trait StringProcessor {
    def process(input: String): String
  }
  val tokenDeleter = new StringProcessor {
    def process(input: String): String = input.replaceAll("[,:;]", "")
  }
  val shortener = new StringProcessor {
    def process(input: String): String =
      if (input.length > 20) input.substring(0, 20).concat("...")
      else input
  }
  val toLowerConvertor = new StringProcessor {
    def process(input: String): String = input.toLowerCase()
  }
  println(shortener.process(toLowerConvertor.process(tokenDeleter.process("This is a Wonderful Test!"))))

  // 7.3 Объекты
  // Задача 1
  class Point(val x: Double, val y: Double, val z: Double)
  object Point {
    def apply(x: Double, y: Double, z: Double): Point = new Point(x, y, z)
    def average(list: List[Point]): Point = {
      if (list.isEmpty) new Point(0, 0, 0)
      else {
        val aX = list.map(p => p.x).sum / list.size
        val aY = list.map(p => p.y).sum / list.size
        val aZ = list.map(p => p.z).sum / list.size
        new Point(aX, aY, aZ)
      }
    }
    def show(point: Point): String = s"${point.x} ${point.y} ${point.z}"
  }
  val l = List(new Point(1,2,3),new Point(7,4,9))
  println(Point.show(Point.average(l)))

  // Задача 2
  object FacedString {
    def apply(input: String) = s"*_*$input*_*"
    def unapply(arg: String): Option[String] = {
      val regex = "\\*_\\*(.*)\\*_\\*".r
      arg match {
        case regex(line) => Some(line)
        case _ => None
      }
    }
  }
  "*_*test*_*" match {
    case FacedString(str) => println(str)
    case _ => println("Could not recognize string")
  }

  // 7.5 Наследование
  trait Animal {
    val sound: String
    def voice(): Unit = println("voice: " + sound)
  }
  class Cat extends Animal {
    val sound: String = "Meow!"
  }
  class Dog extends Animal {
    val sound: String = "Woof!"
  }
  class Fish extends Animal {
    val sound: String = "Fishes are silent!"
    override def voice(): Unit = println(sound)
  }
  val animals = Seq(new Cat, new Dog, new Fish)
  animals.foreach(_.voice())

  // 8.1 Обобщенные типы
  // Задача 2
  case class Pair[T <% Ordered[T]] (first: T, second: T) {
    def smaller: T =
      if (first < second) first
      else second
  }
  val p = Pair(BigInt("1000000000000000"),BigInt("7000000000000000"))
  require(p.smaller == BigInt("1000000000000000"))
}

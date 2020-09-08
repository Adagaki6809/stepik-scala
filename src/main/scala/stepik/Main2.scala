package stepik

import scala.io.StdIn

// 6.2 Коллекции. Часть 2.
// Задача 4
object Main2 extends App {
  import Naval.{Point, Field, Ship, Fleet, Game}
  import Lesson.field
  def validateShip(ship: Ship): Boolean = ship match { // определить, подходит ли корабль по своим характеристикам
    case Nil => false
    case head :: Nil=> true
    case head :: tail => if ((head._1 == tail(0)._1 || head._2 == tail(0)._2) && ship.length <= 4)
      validateShip(tail) else false
  }
  def validatePosition(ship: Ship, field: Field): Boolean = { // определить, можно ли его поставить
  val b = ship.forall(p => p match {
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==0 && y==0) => !field(x)(y) && !field(x+1)(y) && !field(x+1)(y+1) && !field(x)(y+1)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==0 && y==9) => !field(x)(y) && !field(x+1)(y) && !field(x+1)(y-1) && !field(x)(y-1)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==9 && y==0) => !field(x)(y) && !field(x-1)(y) && !field(x-1)(y+1) && !field(x)(y+1)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==9 && y==9) => !field(x)(y) && !field(x-1)(y) && !field(x-1)(y-1) && !field(x)(y-1)

    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==0 && y!=0 && y!=9) => !field(x)(y) && !field(x)(y-1) && !field(x+1)(y-1) && !field(x+1)(y) && !field(x+1)(y+1) && !field(x)(y+1)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && x==9 && y!=0 && y!=9) => !field(x)(y) && !field(x)(y-1) && !field(x-1)(y-1) && !field(x-1)(y) && !field(x-1)(y+1) && !field(x)(y+1)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && y==0 && x!=0 && x!=9) => !field(x)(y) && !field(x-1)(y) && !field(x-1)(y+1) && !field(x)(y+1) && !field(x+1)(y+1) && !field(x+1)(y)
    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9 && y==9 && x!=0 && x!=9) => !field(x)(y) && !field(x-1)(y) && !field(x-1)(y-1) && !field(x)(y-1) && !field(x+1)(y-1) && !field(x+1)(y)

    case (x,y) if (x >= 0 && y >= 0 && x <= 9 && y <= 9) => !field(x)(y) && !field(x-1)(y-1) && !field(x-1)(y) && !field(x-1)(y+1) && !field(x)(y-1) && !field(x)(y+1) && !field(x+1)(y+1) && !field(x+1)(y) && !field(x+1)(y-1)
    case _ => false
  })
    b
  }
  def enrichFleet(fleet: Fleet, name: String, ship: Ship): Fleet = { // добавить корабль во флот
    fleet + (name -> ship)
  }
  def markUsedCells(field: Field, ship: Ship): Field = { // пометить клетки, которые занимает добавляемый корабль
    val r = rec(field,ship)
    r
  }
  def rec(field: Field, ship: Ship): Field = ship match {
    case head :: Nil => field.zipWithIndex.map(r => r._1.zipWithIndex.map(c => if (r._2 == head._1 && c._2 == head._2) true else c._1 || false))
    case head :: tail => rec(field.zipWithIndex.map(r => r._1.zipWithIndex.map(c => if (r._2 == head._1 && c._2 == head._2) true else c._1 || false)),tail)
  }
  def tryAddShip(i:Int, game: Game, name: String, ship: Ship): Game = {  // логика вызовов методов выше
    if (i>0) {
      val a: Array[String] = StdIn.readLine().split(" ")
      lazy val s = Stream.continually(StdIn.readLine()).take(a(1).toInt).map(s => (s.split(" ").apply(0).toInt, s.split(" ").apply(1).toInt)).force.toList

      if (validateShip(s) && validatePosition(s, game._1)) {
        val (newField, newFleet) = (markUsedCells(game._1, s), enrichFleet(game._2, a(0), s))
        tryAddShip(i - 1, (newField, newFleet), a(0), s)
      }
      else
        tryAddShip(i - 1, game, a(0), s)
    } else game
  }

  val i = StdIn.readInt()
  val game = tryAddShip(i, (field, Map()), "", Nil)
  game._2.foreach(ship => println(ship._1))
}


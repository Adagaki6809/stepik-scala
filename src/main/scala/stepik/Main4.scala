package stepik

// 8.4 Неявные параметры
// Задача 1 и Задача 2
object Main4 extends App {

  trait Expr[T] {
    def literalInt(value: Int): T
    def variable(name: String): T
    def times(x: T, y: T): T
    def plus(x: T, y: T): T
    def minus(x: T, y: T): T = plus(x, negate(y))
    def negate(x: T): T = times(x, literalInt(-1))
  }
  object Expr {
    implicit val stringExpr: Expr[String] = new Expr[String] {
      override def literalInt(value: Int): String = s"$value"
      override def variable(name: String): String = s"${name.toUpperCase}"
      override def times(x: String, y: String): String = s"($x)*($y)"
      override def plus(x: String, y: String): String = s"($x)+($y)"
      override def minus(x: String, y: String): String = s"($x)-($y)"
      override def negate(x: String): String = s"-($x)"
    }

    // вот сообственно мой код implicit def numericExpr {...}
    implicit def numericExpr[T: Numeric]: Expr[Calc[T]] = new Expr[Calc[T]] {
      import Numeric.Implicits._
      override def literalInt(value: Int): Calc[T] = _ => implicitly[Numeric[T]].fromInt(value)
      override def variable(name: String): Calc[T] = mapFromFunctionCalcDouble => mapFromFunctionCalcDouble(name)
      override def times(x: Calc[T], y: Calc[T]): Calc[T] = mapFromFunctionCalcDouble => x(mapFromFunctionCalcDouble) * y(mapFromFunctionCalcDouble)
      override def plus(x: Calc[T], y: Calc[T]): Calc[T] = mapFromFunctionCalcDouble => x(mapFromFunctionCalcDouble) + y(mapFromFunctionCalcDouble)
    }

    // часть 2 implicit val stringOrderExpr {...}
    implicit val stringOrderExpr: Expr[Print] = new Expr[Print] {
      override def literalInt(value: Int) = Print(value.toString, 4, true)
      override def variable(name: String): Print =  Print(name.toUpperCase(),5)
      override def times(x: Print, y: Print): Print = x match {
        case Print(_,_,true)  => if (x.priority < 3 && y.priority < 3) Print(s"(${x.s})(${y.s})", 3)
          else if (x.priority < 3) Print(s"(${x.s})${y.s}", 3)
            else if (y.priority < 3) Print(s"${x.s}(${y.s})", 3)
              else Print(s"${x.s}${y.s}", 3)

        case Print(_,_,false) => if (x.priority < 3 && y.priority < 3) Print(s"(${x.s})*(${y.s})", 3)
          else if (x.priority < 3) Print(s"(${x.s})*${y.s}", 3)
            else if (y.priority < 3) Print(s"${x.s}*(${y.s})", 3)
              else Print(s"${x.s}*${y.s}", 3)
      }
      override def plus(x: Print, y: Print): Print  = x match {
        case Print(_, _, true) => if (x.priority < 2 && y.priority < 2) Print(s"(${x.s})+(${y.s})", 2)
        else if (x.priority < 2) Print(s"(${x.s})+${y.s}", 2)
        else if (y.priority < 2) Print(s"${x.s}+(${y.s})", 2)
        else Print(s"${x.s}+${y.s}", 2)

        case Print(_, _, false) => if (x.priority < 2 && y.priority < 2) Print(s"(${x.s})+(${y.s})", 2)
        else if (x.priority < 2) Print(s"(${x.s})+${y.s}", 2)
        else if (y.priority < 2) Print(s"${x.s}+(${y.s})", 2)
        else Print(s"${x.s}+${y.s}", 2)
      }
      override def minus(x: Print, y: Print): Print = x match {
        case Print(_, _, true) => if (x.priority < 2 && y.priority < 2) Print(s"(${x.s})-(${y.s})", 2)
        else if (x.priority < 2) Print(s"(${x.s})-${y.s}", 2)
        else if (y.priority < 2) Print(s"${x.s}-(${y.s})", 2)
        else Print(s"${x.s}-${y.s}", 2)

        case Print(_, _, false) => if (x.priority < 2 && y.priority < 2) Print(s"(${x.s})-(${y.s})", 2)
        else if (x.priority < 2) Print(s"(${x.s})-${y.s}", 2)
        else if (y.priority < 2) Print(s"${x.s}-(${y.s})", 2)
        else Print(s"${x.s}-${y.s}", 2)
      }
      override def negate(x: Print): Print          = Print(s"-${x.s}",1)
    }
  }
  object exprSyntax {
    def literalInt[T](value: Int)(implicit expr: Expr[T]): T = expr.literalInt(value)
    def X[T](implicit expr: Expr[T]): T = expr.variable("x")
    def Y[T](implicit expr: Expr[T]): T = expr.variable("y")
    def Z[T](implicit expr: Expr[T]): T = expr.variable("z")
    implicit class IntToExpr[T](x: Int)(implicit expr: Expr[T]) {
      def lit: T = expr.literalInt(x)
    }

    implicit class NumOps[T](val x: T) extends AnyVal {
      def +(y: T)(implicit expr: Expr[T]): T = expr.plus(x, y)
      def -(y: T)(implicit expr: Expr[T]): T = expr.minus(x, y)
      def *(y: T)(implicit expr: Expr[T]): T = expr.times(x, y)
      def unary_-(implicit expr: Expr[T]): T = expr.negate(x)
    }
  }

  final case class Print(s: String, priority: Int, isLit: Boolean = false) {
    def print(outer: Int = 0) = if (outer <= priority) s else s"($s)"
  }

  import exprSyntax._
  type Calc[T] = Map[String, T] => T
  def function[T: Expr]: T =  X * X + 2.lit * (Y + Z * X * 2.lit) - 7.lit * Z + 4.lit
  println(function[String])                                                               // ((((X)*(X))+((2)*((Y)+(((Z)*(X))*(2)))))-((7)*(Z)))+(4)
  println(function[Calc[Double]].apply(Map("x" -> 1, "y" -> -1, "z" -> 0.2)))             // 2.4
  println(function[Print].print())                                                        // X*X+2(Y+Z*X*2)-7Z+4
}


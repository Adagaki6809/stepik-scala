package stepik

// 8.3 Типы: Псевдонимы и компоненты
object Main3 extends App {
  trait Vect extends Any{
    type Item
    def length: Int
    def get(index: Int): Item
    def set(index: Int, item: Item): Vect.Aux[Item]
  }

  object Vect {
    type Aux[I] = Vect { type Item = I }
  }

  final case class StringVect(str: String) extends AnyVal with Vect {
    type Item = Char
    def length                                 = str.length
    def get(index: Int)                        = str.charAt(index)
    def set(index: Int, item: Char): Vect.Aux[Char] = StringVect(str.updated(index, item))
  }

  final case class BoolVect64(values: Long = 64) extends AnyVal with Vect {
    type Item = Boolean
    def length          = 64

    def get(index: Int) =  {
      if (index >= 0 && index < length && (values & (1L << index)) !=0) true
      else false
    }

    def set(index: Int, item: Boolean): Vect.Aux[Boolean] = index match {
      case index if (index >= 0 && index < length) =>
        if (item) BoolVect64(values | (1L << index))
        else BoolVect64(values & ~(1L << index))
      case _ => BoolVect64(length.toLong)
    }
  }

  final case class BoolVect8(values: Byte = 8) extends AnyVal with Vect {
    type Item = Boolean
    def length          = 8

    def get(index: Int) =  {
      if (index >= 0 && index < length && (values & (1 << index).toByte) !=0 ) true
      else false
    }

    def set(index: Int, item: Boolean): Vect.Aux[Boolean] = index match {
      case index if (index >= 0 && index < length) =>
        if (item) BoolVect8((values | (1 << index)).toByte)
        else BoolVect8((values & ~(1 << index)).toByte)
      case _ => BoolVect8((values & ~(1 << index)).toByte)
    }
  }

  def toList(vect: Vect): List[vect.Item] = (for (i <- 0 until vect.length) yield vect.get(i)).toList

  val s1 = StringVect("abcde").set(0,'F').set(1,'F').set(2,'F')
  println(toList(s1))
  val s2 = BoolVect8(0).set(89,true).set(1,true).set(2,true)
  println(toList(s2))
  val s3 = BoolVect64(0.toLong).set(8,true).set(1,true).set(2,true)
  println(toList(s3))
}

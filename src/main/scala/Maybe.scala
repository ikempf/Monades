import scala.annotation.tailrec

object Maybe {

  // 1
  @tailrec
  def badIndexOf[T](x: T, xs: List[T], from: Int): Int = xs match {
    case h :: t if h == x => from
    case _ :: t => badIndexOf(x, t, from + 1)
    case Nil => ???
  }
  def badIndexOf[T](x: T, xs: List[T]): Int = badIndexOf(x, xs, 0)

  // 2
  sealed abstract class Maybe[+T]

  final case class Just[T](t: T) extends Maybe[T]

  case object Nothing extends Maybe[Nothing]

  def goodIndexOf[T](x: T, xs: List[T], from: Int): Maybe[Int] = xs match {
    case h :: t if h == x => Just(from)
    case _ :: t => goodIndexOf(x, t, from + 1)
    case Nil => Nothing
  }
  def goodIndexOf[T](x: T, xs: List[T]): Maybe[Int] = goodIndexOf(x, xs, 0)



  def main(args: Array[String]): Unit = {
    println(goodIndexOf("c", List("a", "c", "s")))
    println(goodIndexOf("z", List("a", "c", "s")))
    println(badIndexOf("c", List("a", "c", "s")))
    println(badIndexOf("z", List("a", "c", "s")))
  }
}
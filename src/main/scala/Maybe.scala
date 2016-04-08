import scala.annotation.tailrec

object Maybe {

  // 1
  @tailrec
  def badIndexOf[T](x: T, xs: List[T], from: Int): Int = xs match {
    case h :: t if h == x => from
    case _ :: t => badIndexOf(x, t, from + 1)
    case Nil => -1
  }
  def badIndexOf[T](x: T, xs: List[T]): Int = badIndexOf(x, xs, 0)

  // 2
  sealed abstract class Maybe[+A] {
    def map[B](f: A => B): Maybe[B]
    def flatMap[B](f: A => Maybe[B]): Maybe[B]
  }

  case class Just[A](a: A) extends Maybe[A] {
    override def map[B](f: (A) => B): Maybe[B] = Just(f(a))

    override def flatMap[B](f: (A) => Maybe[B]): Maybe[B] = f(a)
  }

  case object Nothing extends Maybe[Nothing] {
    override def map[B](f: (Nothing) => B): Maybe[B] = Nothing

    override def flatMap[B](f: (Nothing) => Maybe[B]): Maybe[B] = Nothing
  }

  @tailrec
  def goodIndexOf[T](x: T, xs: List[T], from: Int): Maybe[Int] = xs match {
    case h :: t if h == x => Just(from)
    case _ :: t => goodIndexOf(x, t, from + 1)
    case Nil => Nothing
  }
  def goodIndexOf[T](x: T, xs: List[T]): Maybe[Int] = goodIndexOf(x, xs, 0)


  def main(args: Array[String]): Unit = {
    // 1
    println("-----Bad style")
    println(badIndexOf("c", List("a", "c", "s")))
    println(badIndexOf("z", List("a", "c", "s")))

    // 2
    println("-----Good style")
    println(goodIndexOf("c", List("a", "c", "s")))
    println(goodIndexOf("z", List("a", "c", "s")))
  }

}
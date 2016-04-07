import java.util.Random

object StateTransitions {

  // 1
  def badStyle(seed: Long) {
    val random = new Random(seed)
    println(random.nextInt())
    println(random.nextInt())
    println(random.nextInt())
  }

  // 2
  final case class RNGState(seed: Long) {
    def next: RNGState = RNGState(seed * 2345678L * 456789L)
  }

  def nextLong(seed: RNGState): (RNGState, Long) =
    (seed.next, seed.seed)

  def main(args: Array[String]): Unit = {
    badStyle(123)
  }

}
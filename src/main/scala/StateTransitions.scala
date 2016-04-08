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
  final case class RNG(seed: Long) {
    def next = RNG(seed * 2345678L * 456789L)
  }

  def nextLong(rng: RNG): (RNG, Long) =
    (rng.next, rng.seed)

  def nextBool(rng: RNG): (RNG, Boolean) =
    (rng.next, rng.seed > 0)

  // 3
  case class State[S, A](run: S => (S, A))



  def main(args: Array[String]): Unit = {
    // 1
    badStyle(123)

    // 2
    val (r1, rand1) = nextLong(RNG(123))
    println(rand1)

    val (r2, rand2) = nextLong(r1)
    println(rand2)

    val (r3, rand3) = nextLong(r2)
    println(rand3)

    // 3

  }

}
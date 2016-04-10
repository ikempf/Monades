import java.util.Random

object State {

  // 1 impure functions
  def badStyle(seed: Long) {
    val random = new Random(seed)
    println(random.nextInt())
    println(random.nextInt())
    println(random.nextInt())
  }

  // 2 state transitions
  final case class RNG(seed: Long) {
    def next = RNG(seed * 2345678L * 456789L)
  }

  def nextLong(rng: RNG): (RNG, Long) =
    (rng.next, rng.seed)

  def nextBool(rng: RNG): (RNG, Boolean) =
    (rng.next, rng.seed > 0)

  case class Transaction(id: Long, amount: Long, system: Boolean)

  def nextTransaction(rng: RNG): (RNG, Transaction) = {
    val (r1, id) = nextLong(rng)
    val (r2, amount) = nextLong(r1)
    val (r3, system) = nextBool(r2)
    (r3, Transaction(id, amount, system))
  }

  // 3 State monad
  case class State[S, A](transition: S => (S, A)) {
    def map[B](f: A => B): State[S, B] =
      State((s) => {
        val (ss, aa) = transition(s)
        (ss, f(aa))
      })

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State((s: S) => {
        val (ss, aa) = transition(s)
        f(aa).transition(s)
      })

    def run(initial :S): A = transition(initial)._2

  }

  def nextLong: State[RNG, Long] =
    State(seed => (seed.next, seed.seed))

  def nextBool: State[RNG, Boolean] =
    State(seed => (seed.next, seed.seed > 0L))

  def nextTransaction: State[RNG, Transaction] = {
    nextLong
      .flatMap(id => nextLong
        .flatMap(amount => nextBool
          .map(system => Transaction(id, amount, system))))
//    for {
//      id <- nextLong
//      amount <- nextLong
//      system <- nextBool
//    } yield Transaction(id, amount, system)
  }


  // Exemple
  def main(args: Array[String]) {
    // 1
    println("-----Bad style")
    badStyle(123)

    // 2
    println("-----State transitions")
    val (rng, t1) = nextTransaction(RNG(123))
    println(t1)

    // 3
    println("-----State monad")
    println(nextTransaction.run(RNG(123)))

  }

}
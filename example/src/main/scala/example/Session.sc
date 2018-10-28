object session {
  def sqrt(x: Double): Double = {

    def sqrt(x: Double, guess: Double): Double = {
      if (isGoodEnough(guess, x)) guess
      else sqrt(x, improveGuess(x, guess))
    }

    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x) < 0.00001 * x

    def improveGuess(d: Double, guess: Double): Double = (guess + d / guess) / 2

    sqrt(x, 1.1)
  }

  def abs(x: Double) = if (x < 0) -x else x

  sqrt(1e60)

  def factorial(x: Int): Int = {

    def factorial(x: Int, acc: Int): Int = {
      if (x == 0) acc
      else factorial(x-1, acc * x)
    }
    factorial(x, 1)
  }
  factorial(4)
}

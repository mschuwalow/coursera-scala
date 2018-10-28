object Solution {
  def backwardsPrime(start: Long, nd: Long): String = {

    def primes(count: Long, acc: List[String]): List[String] = {
      if (count > nd) acc
      else if (isReversedPrime(count)) primes(count + 1, count.toString::acc)
      else primes(count + 1, acc)
    }

    def isReversedPrime(x: Long): Boolean = {
      val rev: Long = x.toString.reverse.toLong
      isPrime(x) && isPrime(rev) && (rev > x)
    }

    def isPrime(n: Long): Boolean = (2 to math.sqrt(n).toInt) forall (x => n % x != 0)

    primes(start, List()).reverse.mkString(",")
  }
  backwardsPrime(7000, 7100)
}
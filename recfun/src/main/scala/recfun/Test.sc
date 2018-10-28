object test {

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a+1, b))
  }

  def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)

  sum(x => x ^ 2)(1, 4)


  def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)

  product(x => x)(1, 3)

  class  Rational(x: Int, y: Int) {
    def numer = x
    def denom = y

    def neg = {
      new Rational(-x , y)
    }

    def add(that: Rational) = {
      new Rational(numer * that.denom + that.numer * denom, denom * that.denom)
    }

    def sub(that: Rational) = {
      add(that.neg)
    }

    override def toString: String = numer + "/" + denom
  }

  val x = new Rational(1, 2)

  def addRationals(x: Rational, y: Rational): Rational = {
    new Rational(x.numer * y.denom + y.numer * x.denom, x.denom * y.denom)
  }
  x.add(new Rational(1, 2))
  x.sub(new Rational(1, 2))

}
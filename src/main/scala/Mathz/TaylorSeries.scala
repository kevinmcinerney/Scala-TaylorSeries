package Mathz

import Mathz._


abstract class TaylorSeries[T](val x: T) {

  def series(n: Int): Stream[BigDecimal]

  def sumOfSeries(series: Stream[BigDecimal]): BigDecimal = series.sum

  def getTaylorSeries(nDecPlaces: Int, degree: Int): BigDecimal

}

  sealed case class Sin(override val x: Radian) extends TaylorSeries(x) {

    override def getTaylorSeries(nDecPlaces: Int, degree: Int): BigDecimal = {
      val series = this.series(0).take(degree / 2 + 1)
      val sum = sumOfSeries(series)//this.sumTaylorSeries(degree / 2 + 1)
      val prevSum = sum + series.last
      if (accurateEnough(sum, prevSum, nDecPlaces)) round(sum, nDecPlaces) else getTaylorSeries(nDecPlaces, degree + 1)
    }
    override def series(n: Int): Stream[BigDecimal] = {
      val v_n = if (2 * (n - 1) - 1 <= 0) 0 else 2 * (n - 1) - 1
      val toDec = BigDecimal(factorial(v_n))
      Stream.cons((pow(-1, n) * pow(Radian(squeeze()).value, v_n)) / toDec, series(n + 1))
    }

    def squeeze(): BigDecimal = {

      def nw_to_ne(deg: BigDecimal): BigDecimal = 180 - deg

      def squeezeHelper(deg: BigDecimal): BigDecimal = deg >= 0 match {
        case true if deg == 0  => 0
        case true if deg < 90  => deg
        case true if deg < 180 => nw_to_ne(deg)
        case true if deg < 270 => -1 * squeezeHelper(deg % 180)
        case true if deg < 360 => -1 * squeezeHelper(nw_to_ne(deg % 180))
        case false => -1 * squeezeHelper(deg.abs)
      }

      squeezeHelper(this.x.degrees % 360)
    }
  }

  sealed case class Cos(override val x: Radian) extends TaylorSeries(x) {

    override def getTaylorSeries(nDecPlaces: Int, degree: Int): BigDecimal = {
      val series = Cos(x).series(0).take(degree / 2 + 1)
      val sum = sumOfSeries(series)
      val prevSum = sum + series.last
      if (accurateEnough(sum, prevSum, nDecPlaces)) round(sum, nDecPlaces) else getTaylorSeries(nDecPlaces, degree + 1)
    }

    override def series(n: Int): Stream[BigDecimal] = {
      val v_n = if (2 * n <= 0) 0 else 2 * n
      val toDec: BigDecimal = BigDecimal(factorial(v_n))
      Stream.cons(pow(-1, n) * pow(x.degrees, v_n) / toDec, series(n + 1))
    }
  }

  sealed case class Euler(override val x: BigDecimal) extends TaylorSeries(x) {

    override def getTaylorSeries(nDecPlaces: Int, degree: Int): BigDecimal = {
      val series = Euler(x).series(0).take(degree + 1)
      val sum = sumOfSeries(series)
      val prevSum = sum + series.last
      if (accurateEnough(sum, prevSum, nDecPlaces)) round(sum, nDecPlaces) else getTaylorSeries(nDecPlaces, degree + 1)
    }

    override def series(n: Int): Stream[BigDecimal] = {
        val toDec: BigDecimal = BigDecimal(factorial(n))
        Stream.cons(pow(x, n) / toDec , series(n + 1))
    }

    def eulerSeries(x: BigDecimal, numerator: BigDecimal, factorial: BigDecimal,
                            n: Int, term: BigDecimal, nDecPlaces: Int, result: BigDecimal): BigDecimal = {
      val next_factorial = if (n <= 1) BigDecimal(1) else n * factorial
      val next_numer = if (n == 0) BigDecimal(1) else numerator * x
      val next_term = next_numer / next_factorial
      val accumResult = result + next_term
      if (accurateEnough(result, accumResult, nDecPlaces)) round(result, nDecPlaces)
      else eulerSeries(x, next_numer, next_factorial, n + 1, next_term, nDecPlaces, accumResult)
    }
  }


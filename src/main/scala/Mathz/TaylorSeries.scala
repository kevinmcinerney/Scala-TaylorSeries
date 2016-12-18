/*
package Mathz

/**
  * Created by kevin on 18/12/16.
  */

import Mathz.{pow, factorial}

abstract class TaylorSeries(val x: Radian) {

  def series(x: BigDecimal, n: Int): Stream[BigDecimal]

}

sealed case class Sin(override val x: Radian) extends TaylorSeries(x) {

  override def series(x: BigDecimal, n: Int): Stream[BigDecimal] = {
    val v_n = if (2 * (n - 1) - 1 <= 0) 0 else 2 * (n - 1) - 1
    val toDec: BigDecimal = BigDecimal(factorial(v_n))
    Stream.cons((pow(-1, n) * pow(x, v_n)) / toDec, series(x, n + 1))
  }

}

sealed case class Cos(override val x: Radian) extends TaylorSeries(x) {

  override def series(x: BigDecimal, n: Int): Stream[BigDecimal] = {
    val v_n = if (2 * n <= 0) 0 else 2 * n
    val toDec: BigDecimal = BigDecimal(factorial(v_n))
    Stream.cons(pow(-1, n) * pow(x, v_n) / toDec, series(x, n + 1))
  }
}


sealed case class Euler(override val x: Radian) extends TaylorSeries(x) {

  override def series(x: BigDecimal, n: Int): Stream[BigDecimal] = {
    val toDec: BigDecimal = BigDecimal(factorial(n))
    Stream.cons(pow(x, n) / toDec , series(x, n + 1))
  }

}*/
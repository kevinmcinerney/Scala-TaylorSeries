package Mathz

/**
  * Created by kevin on 26/11/16.
  */
import Calclator._

abstract class TaylorSeries

case class Sin(x: Double) extends TaylorSeries
case class Cos(x: Double) extends TaylorSeries
case class Euler(x: Double) extends TaylorSeries


class Math {

  def sin(x: Double): Double = recTaylorSeries(Sin(x))(10, 1)
  def sin(x: Double, nDecPlaces: Int): Double = recTaylorSeries(Sin(x))(nDecPlaces, 1)
  def cos(x: Double): Double = recTaylorSeries(Sin(x))(10, 1)
  def cos(x: Double, nDecPlaces: Int): Double = recTaylorSeries(Cos(x))(nDecPlaces, 1)
  def e(x: Double): Double = recTaylorSeries(Sin(x))(10, 1)
  def e(x: Double, nDecPlaces: Int): Double = recTaylorSeries(Euler(x))(nDecPlaces, 1)

  def taylorSeries(t: TaylorSeries): Stream[Double] = t match {
    case Sin(x) => sinSeries(1, x)
    case Cos(x) => cosSeries(0, x)
    case Euler(x) => eulerSeries(0, x)
  }

  def sumTaylorSeries(ts: Stream[Double], degree: Int): Double = {
    ts.take(degree).sum
  }

  def pow(base: Double, exponent: Double): Double = exponent match {
    case 0 => 1
    case _ => base * pow(base, exponent -1)
  }

  def factorial(x: Int): Int = x match{
    case 0 => 1
    case _ => x * factorial(x-1)
  }

  private def recTaylorSeries(t: TaylorSeries)(nDecPlaces: Int, degree: Int): Double = t match {
    case Sin(x) => {
      val cur = sumTaylorSeries(taylorSeries(Sin(x)), degree / 2 + 1)
      val prev = sumTaylorSeries(taylorSeries(Sin(x)), degree / 2)
      if (accurateEnough(cur, prev, nDecPlaces)) cur else recTaylorSeries(Sin(x))(nDecPlaces, degree + 1)
    }
    case Cos(x) => {
      val cur = sumTaylorSeries(taylorSeries(Cos(x)), degree / 2 + 1)
      val prev = sumTaylorSeries(taylorSeries(Cos(x)), degree / 2)
      if (accurateEnough(cur, prev, nDecPlaces)) cur else recTaylorSeries(Cos(x))(nDecPlaces, degree + 1)
    }
    case Euler(x) => {
      val cur = sumTaylorSeries(taylorSeries(Euler(x)), degree + 1)
      val prev = sumTaylorSeries(taylorSeries(Euler(x)), degree)
      if (accurateEnough(cur, prev, nDecPlaces)) cur else recTaylorSeries(Euler(x))(nDecPlaces, degree + 1)
    }

  }

  private def eulerSeries(from: Int, x: Double): Stream[Double] = {
    Stream.cons( pow(x, from) / factorial(from), eulerSeries(from + 1, x))
  }

  private def cosSeries(from: Int, x: Double): Stream[Double] = {
    Stream.cons(pow(-1, from) * pow(x, from) / factorial(from), cosSeries(from + 2, x))
  }

  private def sinSeries(from: Int, x: Double): Stream[Double] = {
    Stream.cons(pow(-1, from) * pow(x, from) / factorial(from), sinSeries(from + 2, x))
  }

  private def accurateEnough(curValue: Double, prevValue: Double, nDecPts: Int): Boolean = {
    val tmp = pow(10, nDecPts)
    val prev = scala.math.round(prevValue * tmp) / tmp
    val cur = scala.math.round(curValue * tmp) / tmp
    scala.math.abs(cur - prev) == 0.0

  }

}

object Math {

  val m = new Math()

  def sin(x: Double): Double = m.sin(x)
  def sin(x: Double, nDecPlaces: Int): Double = m.sin(x, nDecPlaces)
  def cos(x: Double): Double = m.cos(x)
  def cos(x: Double, nDecPlaces: Int): Double = m.cos(x, nDecPlaces)
  def e(x: Double): Double = m.e(x)
  def e(x: Double, nDecPlaces: Int): Double = m.e(x, nDecPlaces)
m
  def pow(base: Double, exponent: Double): Double = m.pow(base, exponent)

  def factorial(x: Int): Int = m.factorial(x)

  def taylorSeries(t: TaylorSeries): Stream[Double] = m.taylorSeries(t)

  def sumTaylorSeries(ts: Stream[Double], degree: Int): Double = m.sumTaylorSeries(ts, degree)

}


object MathsRunner {

  def main(args: Array[String]): Unit = {



  }
}





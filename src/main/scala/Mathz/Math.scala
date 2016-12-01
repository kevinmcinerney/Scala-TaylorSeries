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
    case Sin(x) => sinSeries(1, x, 0)
    case Cos(x) => cosSeries(0, x, 0)
    case Euler(x) => eulerSeries(0, x)
  }

  def sumTaylorSeries(ts: Stream[Double], degree: Int): Double = {
    ts.take(degree).sum
  }

  def sumTaylorSeries(ts: Stream[Double]): Double = {
    ts.sum
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
      lazy val cur = taylorSeries(Sin(x)).take(degree / 2 + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum,nDecPlaces)  else recTaylorSeries(Sin(x))(nDecPlaces, degree + 1)
    }
    case Cos(x) => {
      lazy val cur = taylorSeries(Cos(x)).take(degree / 2 + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum,nDecPlaces) else recTaylorSeries(Cos(x))(nDecPlaces, degree + 1)
    }
    case Euler(x) => {
      lazy val cur = taylorSeries(Euler(x)).take(degree / 2 + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum, nDecPlaces) else recTaylorSeries(Euler(x))(nDecPlaces, degree + 1)
    }
  }

  private def eulerSeries(from: Int, x: Double): Stream[Double] = {
    Stream.cons( pow(x, from) / factorial(from), eulerSeries(from + 1, x))
  }

  private def cosSeries(from: Int, x: Double, n: Int): Stream[Double] = {
    Stream.cons(pow(-1, n) * pow(x, from) / factorial(from), cosSeries(from + 2, x, n + 1))
  }

  private def sinSeries(from: Int, x: Double, n: Int): Stream[Double] = {
    Stream.cons((pow(-1, n) * pow(x, from)) / factorial(from), sinSeries(from + 2, x, n + 1))
  }

  private def accurateEnough(curValue: Double, prevValue: Double, nDecPts: Int): Boolean = {
    val prev = round(prevValue, nDecPts)
    val cur =  round(curValue, nDecPts)
    scala.math.abs(cur - prev) == 0.0

  }

  private def round(value: Double, nDecPts: Int): Double = {
    val tmp = pow(10, nDecPts)
    scala.math.round(value * tmp) / tmp
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

  def pow(base: Double, exponent: Double): Double = m.pow(base, exponent)

  def factorial(x: Int): Int = m.factorial(x)

  def taylorSeries(t: TaylorSeries): Stream[Double] = m.taylorSeries(t)

  def sumTaylorSeries(ts: Stream[Double], degree: Int): Double = m.sumTaylorSeries(ts, degree)

}


object MathsRunner {

  def main(args: Array[String]): Unit = {



  }
}





package TaylorSeries

/**
  * Created by kevin on 26/11/16.
  */

class TaylorSeries {

  abstract class TaylorFunction

  case class Sin(x: Double) extends TaylorFunction
  case class Cos(x: Double) extends TaylorFunction
  case class E(x: Double) extends TaylorFunction


  def calculate(f: TaylorFunction, degree: Int): Double = f match {
    case Sin(x: Double) => sin(x, 1, degree, true)
    case Cos(x: Double) => 0f
    case E(x: Double) => 0f
  }

  def sin(x: Double, n: Int, d: Int, positive: Boolean): Double = positive match {

    case false if n == d => (1.0 / factorial(n)) * Math.pow(x, n)
    case true  if n == d => (1.0 / factorial(n)) * Math.pow(x, n)
    case false => (1.0 / factorial(n)) * Math.pow(x, n) + sin(x, n + 2, d, positive = true)
    case true  => (1.0 / factorial(n)) * Math.pow(x, n) - sin(x, n + 2, d, positive = false)

  }

  def cos(x: Int, degree: Int): Float = {
    ???
  }

  def e(x: Int, degree: Int): Float = {
    ???
  }

  def factorial(x: Int): Int = x match{
    case 0 => 1
    case _ => x * factorial(x-1)
  }

}

object TaylorSeriesRunner {

  def main(args: Array[String]): Unit = {

    val ts = new TaylorSeries()

    println(" = " + ts.calculate(ts.Sin(0.9), 15))
  }
}





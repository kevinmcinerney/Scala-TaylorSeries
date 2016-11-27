package TaylorSeries

/**
  * Created by kevin on 26/11/16.
  */
import Math._

abstract class TaylorFunction(val base: Int)

case class Sin(x: Double) extends TaylorFunction(1)
case class Cos(x: Double) extends TaylorFunction(0)
case class E(x: Double) extends TaylorFunction(0)

object TaylorSeries {

  def calculate(t: TaylorFunction, nDecPlaces: Int, degrees: Int): Double = {

    if(degrees - 2 < 0){
      val cur = calculateHelper(t, degrees + 2)
      val prev = calculateHelper(t, degrees)
      if (accurateEnough(cur, prev, nDecPlaces)) cur else calculate(t, nDecPlaces, degrees + 2)
    }else{
      val cur = calculateHelper(t, degrees)
      val prev = calculateHelper(t, degrees - 2)
      if (accurateEnough(cur, prev, nDecPlaces)) cur else calculate(t, nDecPlaces, degrees + 2)
    }


  }

 private def calculateHelper(t: TaylorFunction, degree: Int) : Double = t match {
    case Sin(x: Double) => sin(x, t.base, degree, positive = true)
    case Cos(x: Double) => cos(x, t.base, degree, positive = true)
    case E(x: Double) => 0f
  }


  def sin(x: Double, n: Int, d: Int, positive: Boolean): Double = positive match {

    case false if n >= d => {println("d = " + n); (1.0 / factorial(n)) * pow(x, n)}
    case true  if n >= d => {println("d = " + n); (1.0 / factorial(n)) * pow(x, n)}
    case false => {println("d = " + n); (1.0 / factorial(n)) * pow(x, n) + sin(x, n + 2, d, positive = true)}
    case true  => {println("d = " + n); (1.0 / factorial(n)) * pow(x, n) - sin(x, n + 2, d, positive = false)}

  }

  def cos(x: Double, n: Int, d: Int, positive: Boolean): Double = positive match {

    case false if n >= d => (1.0 / factorial(n)) * pow(x, n)
    case true  if n >= d => (1.0 / factorial(n)) * pow(x, n)
    case false => (1.0 / factorial(n)) * pow(x, n) + cos(x, n + 2, d, positive = true)
    case true  => (1.0 / factorial(n)) * pow(x, n) - cos(x, n + 2, d, positive = false)
  }

  def e(x: Int, degree: Int): Float = {
    ???
  }

  def factorial(x: Int): Int = x match{
    case 0 => 1
    case _ => x * factorial(x-1)
  }

  def accurateEnough(curValue: Double, prevValue: Double, nDecPts: Int): Boolean = {
    val tmp = pow(10, nDecPts)
    val prev = round(prevValue * tmp) / tmp
    val cur = round(curValue * tmp) / tmp
    println("The difference between f(n): " + cur + " and g(n): " + prev + " = " +  abs(cur - prev))
    abs(cur - prev) == 0.0

  }

}

object TaylorSeriesRunner {

  def main(args: Array[String]): Unit = {

    val ts = TaylorSeries

    println(" = " + ts.calculate(Sin(0.75), 10, 1))

    println(ts.accurateEnough(0.1233, 0.1234, 4))


  }
}





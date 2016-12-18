package Mathz

/**
  * Created by kevin on 26/11/16.
  */
import java.math.{MathContext, RoundingMode}

import org.scalameter._



abstract class TaylorSeries

sealed case class Sin(x: Radian) extends TaylorSeries

sealed case class Cos(x: Radian) extends TaylorSeries

sealed case class Euler(x: BigDecimal) extends TaylorSeries


class Radian private (val degrees: Double) {

  private val PI = Math.PI

  val value = degrees * PI / 180

  def +(other: Radian) = Radian(this.value + other.value)

  def -(other: Radian) = Radian(this.value - other.value)

  def *(by: Double) = Radian(this.value * by)

  def /(by: Double) = Radian(this.value / by)

  def %(other: Radian) = Radian(this.value / other.value)

  override def toString: String = "("+degrees + " * PI) / " + 180 + ")"

}

object Radian { def apply(degree: Double) = new Radian(degree) }


class Mathz private {


  /*
  This helper method transforms degrees into the range (PI/4 to -PI/4)
  to make the sin calculation more efficient.
    _________
   /         \       LAWS
  /  nw   ne  \      1) NW, SW,& SE can all be derived from NE Quadrant
 |             |     2) The magnitude of sin is equal for two angles that
 |             |        that move in equal, but opposite directions from zero.
  \  sw   se  /         However, there sign will be opposite
   \_________/

   */

  private def squeeze(deg: Double): Double = {

    def nw_to_ne(deg: Double): Double = 180 - deg

    def squeezeHelper(deg: Double): Double = deg >= 0 match {
        case true if deg == 0  => 0
        case true if deg < 90  => deg
        case true if deg < 180 => nw_to_ne(deg)
        case true if deg < 270 => -1 * squeeze(deg % 180)
        case true if deg < 360 => -1 * squeeze(nw_to_ne(deg % 180))
        case false => -1 * squeeze(deg.abs)
      }

      squeezeHelper(deg % 360)
  }


  /*def taylorSeries(t: TaylorSeries): Stream[BigDecimal] = {
    case Sin(x) => t.series(x.value, 0)
  }*/

  def taylorSeries(t: TaylorSeries): Stream[BigDecimal] = t match {
    case Sin(x) => sinSeries(x.value, 0)
    case Cos(x) => cosSeries(x.value.abs, 0)
    case Euler(x) => eulerSeries(x, 0)
  }


  def sumTaylorSeries(ts: Stream[BigDecimal], degree: Int): BigDecimal = ts.take(degree).sum

  def sumTaylorSeries(ts: Stream[BigDecimal]): BigDecimal = ts.sum

  def pow(base: BigDecimal, exponent: BigInt): BigDecimal = {
    if (exponent == BigInt(0)) BigDecimal(1)
    else base * pow(base, exponent - 1)
  }

  def factorial(x: BigInt): BigInt = {
    if (x == BigInt(0)) 1
    else x * factorial(x - 1)
  }

  private def recTaylorSeries(t: TaylorSeries)(nDecPlaces: Int, degree: Int): BigDecimal = t match {
    case Sin(x) => {
      val cur = taylorSeries(Sin(x)).take(degree / 2 + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum, nDecPlaces) else recTaylorSeries(Sin(x))(nDecPlaces, degree + 1)
    }
    case Cos(x) => {
      val cur = taylorSeries(Cos(x)).take(degree / 2 + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum, nDecPlaces) else recTaylorSeries(Cos(x))(nDecPlaces, degree + 1)
    }
    case Euler(x) => {
      val cur = taylorSeries(Euler(x)).take(degree + 1)
      val curSum = sumTaylorSeries(cur)
      val prevSum = curSum + cur.last
      if (accurateEnough(curSum, prevSum, nDecPlaces)) round(curSum, nDecPlaces) else recTaylorSeries(Euler(x))(nDecPlaces, degree + 1)
    }
  }


  private def sinSeries(x: BigDecimal, n: Int): Stream[BigDecimal] = {
    val v_n = if (2 * (n - 1) - 1 <= 0) 0 else 2 * (n - 1) - 1
    val toDec: BigDecimal = BigDecimal(factorial(v_n))
    Stream.cons((pow(-1, n) * pow(x, v_n)) / toDec, sinSeries(x, n + 1))
  }

  private def cosSeries(x: BigDecimal, n: Int): Stream[BigDecimal] = {
    val v_n = if (2 * n <= 0) 0 else 2 * n
    val toDec: BigDecimal = BigDecimal(factorial(v_n))
    Stream.cons(pow(-1, n) * pow(x, v_n) / toDec, cosSeries(x, n + 1))
  }

  private def eulerSeries(x: BigDecimal, n: BigInt): Stream[BigDecimal] = {
    val toDec: BigDecimal = BigDecimal(factorial(n))
    Stream.cons(pow(x, n) / toDec , eulerSeries(x, n + 1))
  }

  //                      1          1                  1               2       1             5
  private def eulerSeries(x: BigDecimal, numerator: BigDecimal, factorial: BigDecimal,
                          n: Int, term: BigDecimal, nDecPlaces: Int, result: BigDecimal): BigDecimal = {
    val next_factorial = if (n <= 1) BigDecimal(1) else n * factorial
    val next_numer = if (n == 0) BigDecimal(1) else numerator * x
    val next_term = next_numer / next_factorial
    val accumResult = result + next_term
    if (accurateEnough(result, accumResult, nDecPlaces)) round(result, nDecPlaces)
    else eulerSeries(x, next_numer, next_factorial, n + 1, next_term, nDecPlaces, accumResult)

  }

  private def accurateEnough(curValue: BigDecimal, prevValue: BigDecimal, nDecPts: Int): Boolean = {

    val prev = round(prevValue, nDecPts + 2)
    val cur = round(curValue, nDecPts + 2)
    (cur - prev).abs == 0.0
  }

  def round(n: BigDecimal, p: Int): BigDecimal = {
    n.round(new MathContext(p, RoundingMode.HALF_UP))
  }

}

object Mathz {

  val m = new Mathz()

  def sin(x: Radian): BigDecimal = m.recTaylorSeries(Sin(Radian(squeeze(x.degrees))))(10, 1)
  def sin(x: Radian, nDecPlaces: Int): BigDecimal = m.recTaylorSeries(Sin(Radian(squeeze(x.degrees))))(nDecPlaces, 10)
  def cos(x: Radian): BigDecimal = m.recTaylorSeries(Cos(Radian(squeeze(x.degrees))))(10, 1)
  def cos(x: Radian, nDecPlaces: Int): BigDecimal = m.recTaylorSeries(Cos(Radian(squeeze(x.degrees))))(nDecPlaces, 10)
  def e(x: BigDecimal): BigDecimal = m.recTaylorSeries(Euler(x))(5, 1)
  def e(x: BigDecimal, nDecPlaces: Int, mem: Boolean): BigDecimal = {
    if (mem) m.eulerSeries(x, 0, 0, 0, 0, nDecPlaces, 0) else m.recTaylorSeries(Euler(x))(nDecPlaces, 5)
  }

  def pow(base: BigDecimal, exponent: Int): BigDecimal = m.pow(base, exponent)

  def factorial(x: BigInt): BigInt = m.factorial(x)

  def taylorSeries(t: TaylorSeries): Stream[BigDecimal] = m.taylorSeries(t)

  def sumTaylorSeries(ts: Stream[BigDecimal], degree: Int): BigDecimal = m.sumTaylorSeries(ts, degree)

  def round(n: BigDecimal, p:Int) = m.round(n, p)

  def squeeze(x: Double): Double = m.squeeze(x)

}


object MathzRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)


  def main(args: Array[String]): Unit = {

    val m = Mathz

    val noMemory = standardConfig measure m.e(BigDecimal(1), 5, mem = false)

    val memory = standardConfig measure  m.e(BigDecimal(1), 5, mem = true)

    val speedUp = noMemory.value / memory.value

    println("========================= e Time ================================== Value")
    println()
    print(s"Memory time:".padTo(25, ' '))
    print(s"$memory".padTo(40, ' '))
    println(m.e(BigDecimal(0.3), 10, false).toString.padTo(25, ' '))
    print(s"No Memory time:".padTo(25, ' '))
    print(s"$noMemory".padTo(40, ' '))
    println(m.e(BigDecimal(0.3), 10, true).toString.padTo(25, ' '))
    print(s"Speed up:".padTo(25, ' '))
    print(s"$speedUp".padTo(25, ' '))
    println()
    println()
    println("========================================================================")

    val lib = standardConfig measure Math.sin(0.3)

    val mine = standardConfig measure  m.sin(Radian(0.3))

    val speedUp2 = lib.value / mine.value

    println()
    println("========================= Sin Time ================================== Value")
    println()
    print(s"Mine:".padTo(25, ' '))
    print(s"$mine".padTo(40, ' '))
    println(m.sin(Radian(0.3), 10).toString.padTo(25, ' '))
    print(s"Library:".padTo(25, ' '))
    print(s"$lib".padTo(40, ' '))
    println(Math.sin(0.3)).toString.padTo(25, ' ')

    print(s"Speed up:".padTo(25, ' '))
    print(s"$speedUp2".padTo(25, ' '))
    println()
    println()
    println("========================================================================")
  }
}





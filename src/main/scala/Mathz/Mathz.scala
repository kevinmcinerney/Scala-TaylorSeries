package Mathz

import java.math.{MathContext, RoundingMode}

import org.scalameter._


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



  private def pow(base: BigDecimal, exponent: BigInt): BigDecimal = {
    if (exponent == BigInt(0)) BigDecimal(1)
    else base * pow(base, exponent - 1)
  }

  private def factorial(x: BigInt): BigInt = {
    if (x == BigInt(0)) 1
    else x * factorial(x - 1)
  }

  private def accurateEnough(curValue: BigDecimal, prevValue: BigDecimal, nDecPts: Int): Boolean = {

    val prev = round(prevValue, nDecPts + 2)
    val cur = round(curValue, nDecPts + 2)
    (cur - prev).abs == 0.0
  }


  private def round(n: BigDecimal, p: Int): BigDecimal = {
    n.round(new MathContext(p, RoundingMode.HALF_UP))
  }

}

object Mathz {

  val m = new Mathz()
  val s = Sin
  val c = Cos
  val e = Euler

  def sin(x: Radian):                  BigDecimal = Sin(x).getTaylorSeries(10, 1)
  def sin(x: Radian, nDecPlaces: Int): BigDecimal = Sin(x).getTaylorSeries(nDecPlaces, 10)
  def cos(x: Radian):                  BigDecimal = Cos(x).getTaylorSeries(10, 1)
  def cos(x: Radian, nDecPlaces: Int): BigDecimal = Cos(x).getTaylorSeries(nDecPlaces, 10)
  def e(x: BigDecimal):                BigDecimal = Euler(x).getTaylorSeries(5, 1)
  def e(x: BigDecimal, nDecPlaces: Int, mem: Boolean): BigDecimal = {
    if (mem) Euler(x).eulerSeries(x, 0, 0, 0, 0, nDecPlaces, 0)
    else Euler(x).getTaylorSeries(nDecPlaces, 5)
  }

  def pow(base: BigDecimal, exponent: Int): BigDecimal = m.pow(base, exponent)

  def factorial(x: BigInt): BigInt = m.factorial(x)

  def round(n: BigDecimal, p: Int): BigDecimal = m.round(n, p)

  //def squeeze(x: Double): Double = m.squeeze(x)

  def accurateEnough(curValue: BigDecimal, prevValue: BigDecimal, nDecPts: Int): Boolean = m.accurateEnough(curValue, prevValue, nDecPts)

}


object MathzRunner {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> false
  ) withWarmer(new Warmer.Default)


  def main(args: Array[String]): Unit = {

    /*val m = Mathz

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

    val lib = standardConfig measure Math.sin(45)

    val mine = standardConfig measure  m.sin(Radian(45))

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
    println("========================================================================")*/

    println(Radian(89).value)
    println(Mathz.sin(Radian(89)))
  }
}





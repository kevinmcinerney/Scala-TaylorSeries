/**
  * Created by kevin on 26/11/16.
  */


import Mathz.Mathz._
import Mathz.{Radian, Sin}
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {

  import Mathz.Mathz._

  val eta = 0.001

  def ~=(x: BigDecimal, y: BigDecimal) = if ((x - y).abs < eta) true else false


  test("Sin values are correct about zero") {

    assert(~=(sin(new Radian(10), 10), Math.sin(new Radian(10).value.toDouble)))
    assert(~=(sin(new Radian(20), 10), Math.sin(new Radian(20).value.toDouble)))
    assert(~=(sin(new Radian(45), 10), Math.sin(new Radian(45).value.toDouble)))
    assert(~=(sin(new Radian(65), 5), Math.sin(new Radian(65).value.toDouble)))
    assert(~=(sin(new Radian(89), 3), Math.sin(new Radian(89).value.toDouble)))
    assert(~=(sin(new Radian(-10), 10), Math.sin(new Radian(-10).value.toDouble)))
    assert(~=(sin(new Radian(-20), 10), Math.sin(new Radian(-20).value.toDouble)))
    assert(~=(sin(new Radian(-45), 10), Math.sin(new Radian(-45).value.toDouble)))
    assert(~=(sin(new Radian(-65), 5), Math.sin(new Radian(-65).value.toDouble)))
    assert(~=(sin(new Radian(-89), 3), Math.sin(new Radian(-89).value.toDouble)))
  }

  test("Sin gives correct number of decimal places") {

    assert(~=(sin(new Radian(10), 2).toString().split('.')(1).length, 2))
    assert(~=(sin(new Radian(20), 3).toString().split('.')(1).length, 3))
    assert(~=(sin(new Radian(30), 4).toString().split('.')(1).length, 4))
    assert(~=(sin(new Radian(40), 5).toString().split('.')(1).length, 5))
    assert(~=(sin(new Radian(10), 6).toString().split('.')(1).length, 6))
    assert(~=(sin(new Radian(20), 7).toString().split('.')(1).length, 7))
    assert(~=(sin(new Radian(30), 8).toString().split('.')(1).length, 8))
    assert(~=(sin(new Radian(40), 9).toString().split('.')(1).length, 9))
  }



  test("Cos values are correct about zero") {

    val eta = 0.001
    assert(~=(cos(new Radian(10), 10), Math.cos(new Radian(10).value.toDouble)))
    assert(~=(cos(new Radian(20), 10), Math.cos(new Radian(20).value.toDouble)))
    assert(~=(cos(new Radian(30), 10), Math.cos(new Radian(30).value.toDouble)))
    assert(~=(cos(new Radian(40), 7), Math.cos(new Radian(40).value.toDouble)))
    assert(~=(cos(new Radian(50), 5), Math.cos(new Radian(50).value.toDouble)))
    assert(~=(cos(new Radian(60), 3), Math.cos(new Radian(60).value.toDouble)))
    assert(~=(cos(new Radian(-10), 10), Math.cos(new Radian(10).value.toDouble)))
    assert(~=(cos(new Radian(-20), 10), Math.cos(new Radian(-20).value.toDouble)))
    assert(~=(cos(new Radian(-30), 10), Math.cos(new Radian(-30).value.toDouble)))
    assert(~=(cos(new Radian(-40), 7), Math.cos(new Radian(-40).value.toDouble)))
    assert(~=(cos(new Radian(-50), 5), Math.cos(new Radian(-50).value.toDouble)))
    assert(~=(cos(new Radian(-60), 3), Math.cos(new Radian(-60).value.toDouble)))
  }

  test("E values are correct about zero") {

    assert(~=(e(0.2), Math.exp(0.2)))
    assert(~=(e(0.3), Math.exp(0.3)))
    assert(~=(e(0.4), Math.exp(0.4)))
    assert(~=(e(0.5), Math.exp(0.5)))
    assert(~=(e(0.6), Math.exp(0.6)))
  }

  test("E values without mem are correct about zero") {

    assert(~=(e(0.2,10, mem = false), Math.exp(0.2)))
    assert(~=(e(0.3,10, mem = false), Math.exp(0.3)))
    assert(~=(e(0.4,10, mem = false), Math.exp(0.4)))
    assert(~=(e(0.5,10, mem = false), Math.exp(0.5)))
    assert(~=(e(0.6,10, mem = false), Math.exp(0.6)))
    assert(~=(e(1,  5,  mem = false), Math.exp(1)))
    assert(~=(e(1,  5,  mem = false), Math.exp(1)))
  }

  test("E values with mem are correct about zero") {

    assert(~=(e(0.2,10, mem = true), Math.exp(0.2)))
    assert(~=(e(0.3,10, mem = true), Math.exp(0.3)))
    assert(~=(e(0.4,10, mem = true), Math.exp(0.4)))
    assert(~=(e(0.5,10, mem = true), Math.exp(0.5)))
    assert(~=(e(0.6,10, mem = true), Math.exp(0.6)))
    assert(~=(e(1,  5,  mem = true), Math.exp(1)))
    assert(~=(e(1,  5,  mem = true), Math.exp(1)))

  }

  test("factorial is correct about zero") {

    assert(factorial(0) == 1)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

  test("TaylorPolynomial is correct about zero") {
    val rad30 = new Radian(BigDecimal(30))
    assert(taylorSeries(Sin(rad30)).take(10).sum == sumTaylorSeries( taylorSeries( Sin(rad30)), 10))
  }

  test("pow is correct") {

    assert(pow(-1, 0) == 1)
    assert(pow(-1, 1) == -1)
    assert(pow(-1, 2) == 1)
    assert(pow(-1, 3) == -1)
  }


  test("Radian class is correct") {
    import Mathz.Radian
    val rad360 = new Radian(BigDecimal(360))
    val rad180 = new Radian(BigDecimal(180))
    val neg_rad180 = new Radian(BigDecimal(-180))
    val rad361 = new Radian(BigDecimal(361))
    val rad120 = new Radian(BigDecimal(120))
    val neg_rad120 = new Radian(BigDecimal(-120))
    val rad90 = new  Radian(BigDecimal(90))
    val rad30 = new Radian(BigDecimal(30))
    val neg_rad30 = new Radian(BigDecimal(-30))
    val rad1 = new Radian(BigDecimal(1))
    val rad0 = new Radian(BigDecimal(0))

    //
    assert(~=(new Radian(180).degrees, 180))
    assert(~=(new Radian(180).value, Math.PI))
    assert(~=(new Radian(90).value, 2 *Math.PI / 4))

    // = - * /
    assert(~=((rad30 + rad30).value, (rad90 - rad30).value))
    assert(~=((rad30 * 2).value, (rad120 / 2).value))
    assert(~=(rad30.degrees, 30))

    //to Range
    assert(~=(rad360.toRange(rad180).value, rad0.value))
    assert(~=(rad180.toRange(rad360).value, rad180.value))
    assert(~=(neg_rad180.toRange(rad360).value, neg_rad180.value))
    assert(~=(rad361.toRange(rad360).value, rad1.value))
    assert(~=(rad120.toRange(rad90).value, rad30.value))
    assert(~=(neg_rad120.toRange(rad90).value, neg_rad30.value))

  }
}




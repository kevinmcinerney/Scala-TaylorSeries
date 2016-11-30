/**
  * Created by kevin on 26/11/16.
  */


import Mathz.Math._
import Mathz.Sin
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {


  import Mathz.Math._

  test("Sin values are correct") {

    val eta = 0.001
    assert(sin(0.2) - Math.sin(0.2) < eta)
    assert(sin(0.3) - Math.sin(0.3) < eta)
    assert(sin(0.4) - Math.sin(0.4) < eta)
    assert(sin(0.5) - Math.sin(0.5) < eta)
    assert(sin(0.7) - Math.sin(0.7) < eta)
  }

  test("Cos values are correct") {

    val eta = 0.001
    assert(cos(0.2) - Math.cos(0.2) < eta)
    assert(cos(0.3) - Math.cos(0.3) < eta)
    assert(cos(0.4) - Math.cos(0.4) < eta)
    assert(cos(0.5) - Math.cos(0.5) < eta)
    assert(cos(0.7) - Math.cos(0.7) < eta)
  }

  test("E values are correct") {

    val eta = 0.001
    assert(e(1) - Math.exp(1) < eta)
    assert(e(2) - Math.exp(2) < eta)
    assert(e(3) - Math.exp(3) < eta)
    assert(e(4) - Math.exp(4) < eta)
    assert(e(5) - Math.exp(5) < eta)
  }

  test("factorial is correct") {

    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

  test("TaylorPolynomial is correct") {

    assert(taylorSeries(Sin(0.2)).take((10)).sum == sumTaylorSeries( taylorSeries( Sin(0.2) ), 10))
    assert(sin(0.2,10) == sumTaylorSeries( taylorSeries( Sin(0.2) ), 5))

  }
}




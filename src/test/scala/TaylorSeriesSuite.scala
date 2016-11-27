/**
  * Created by kevin on 26/11/16.
  */


import TaylorSeries._
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner


@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {

  val ts = TaylorSeries
  import ts._

  test("Sin values are correct") {

    val eta = 0.001
    assert(calculate(Sin(0.2), 3, 1) - Math.sin(0.2) < eta)
    assert(calculate(Sin(0.3), 3, 1) - Math.sin(0.3) < eta)
    assert(calculate(Sin(0.4), 3, 1) - Math.sin(0.4) < eta)
    assert(calculate(Sin(0.5), 3, 1) - Math.sin(0.5) < eta)
    assert(calculate(Sin(0.7), 3, 1) - Math.sin(0.7) < eta)
  }

  test("Cos values are correct") {

    val eta = 0.001
    assert(calculate(Cos(0.2), 3, 1) - Math.cos(0.2) < eta)
    assert(calculate(Cos(0.3), 3, 1) - Math.cos(0.3) < eta)
    assert(calculate(Cos(0.4), 3, 1) - Math.cos(0.4) < eta)
    assert(calculate(Cos(0.5), 3, 1) - Math.cos(0.5) < eta)
    assert(calculate(Cos(0.7), 3, 1) - Math.cos(0.7) < eta)
  }

  test("factorial is correct") {

    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

  test("accurateEnough is correct") {

    assert(accurateEnough(0.123, 0.1234, 0))
    assert(accurateEnough(0.123, 0.1234, 1))
    assert(accurateEnough(0.123, 0.1234, 2))
    assert(accurateEnough(0.123, 0.1234, 3))
    assert(!accurateEnough(0.1233, 0.1234, 4))
    assert(!accurateEnough(0.12333, 0.12344, 5))
    assert(!accurateEnough(0.123333, 0.123444, 6))



  }

}




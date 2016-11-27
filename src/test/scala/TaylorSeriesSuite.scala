/**
  * Created by kevin on 26/11/16.
  */


import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

object TS extends TaylorSeries.TaylorSeries
import TS._

@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {

  test("Sin values are correct") {

    val eta = 0.001
    assert(calculate(Sin(0.2), 9) - Math.sin(0.2) < eta)
    assert(calculate(Sin(0.3), 9) - Math.sin(0.3) < eta)
    assert(calculate(Sin(0.4), 9) - Math.sin(0.4) < eta)
    assert(calculate(Sin(0.5), 9) - Math.sin(0.5) < eta)
    assert(calculate(Sin(0.7), 9) - Math.sin(0.7) < eta)
  }

  test("factorial is correct") {

    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

}




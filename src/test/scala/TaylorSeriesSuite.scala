/**
  * Created by kevin on 26/11/16.
  */


import Mathz.Mathz._
import Mathz.Sin
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {


  import Mathz.Mathz._

  test("Sin values are correct about zero") {

    val eta = 0.001
    assert((sin(0.2, 10) - Math.sin(0.2)).abs < eta)
    assert((sin(0.3, 10) - Math.sin(0.3)).abs < eta)
    assert((sin(0.4, 10) - Math.sin(0.4)).abs < eta)
    assert((sin(0.5, 10) - Math.sin(0.5)).abs < eta)
    assert((sin(0.7, 10) - Math.sin(0.7)).abs < eta)
  }

  test("Sin gives correct number of decimal places") {

    val eta = 0.001
    assert(sin(0.3, 2).toString.length == 4)
    assert(sin(0.4, 3).toString.length == 5)
    assert(sin(0.5, 4).toString.length == 6)
    assert(sin(0.7, 5).toString.length == 7)
  }



  test("Cos values are correct about zero") {

    val eta = 0.001
    assert(cos(0.2, 16) == Math.cos(0.2))
    assert((cos(0.2, 10) - Math.cos(0.2)).abs < eta)
    assert((cos(0.3, 10) - Math.cos(0.3)).abs < eta)
    assert((cos(0.4, 10) - Math.cos(0.4)).abs < eta)
    assert((cos(0.5, 10) - Math.cos(0.5)).abs < eta)
    assert((cos(0.7, 10) - Math.cos(0.7)).abs < eta)
  }

  test("E values are correct about zero") {

    val eta = 0.001
    assert((e(0.2) - Math.exp(0.2)).abs < eta)
    assert((e(0.3) - Math.exp(0.3)).abs < eta)
    assert((e(0.4) - Math.exp(0.4)).abs < eta)
    assert((e(0.5) - Math.exp(0.5)).abs < eta)
    assert((e(0.6) - Math.exp(0.6)).abs < eta)
  }

  test("E values without mem are correct about zero") {

         val eta = 0.1
         assert((e(0.2,10, false) - Math.exp(0.2)).abs < eta / 10)
         assert((e(0.3,10, false) - Math.exp(0.3)).abs < eta / 10)
         assert((e(0.4,10, false) - Math.exp(0.4)).abs < eta / 10)
         assert((e(0.5,10, false) - Math.exp(0.5)).abs < eta / 10)
         assert((e(0.6,10, false) - Math.exp(0.6)).abs < eta / 10)
         assert((e(1,  2,  false) - Math.exp(1)).abs   < eta / 2)
         assert((e(1,  5,  false) - Math.exp(1)).abs   < eta / 5)
       }

  test("E values with mem are correct about zero") {

    val eta = 0.1
    assert((e(0.2,10, true)  - Math.exp(0.2)).abs < eta / 10)
    assert((e(0.3,10, true) - Math.exp(0.3)).abs  < eta / 10)
    assert((e(0.4,10, true) - Math.exp(0.4)).abs  < eta / 10)
    assert((e(0.5,10, true) - Math.exp(0.5)).abs  < eta / 10)
    assert((e(0.6,10, true) - Math.exp(0.6)).abs  < eta / 10)
    assert((e(1,  2,  true) - Math.exp(1)).abs    < eta / 2)
    assert((e(1,  5,  true) - Math.exp(1)).abs    < eta / 5)

  }

  test("factorial is correct about zero") {

    assert(factorial(0) == 1)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

  test("TaylorPolynomial is correct about zero") {

    assert(taylorSeries(Sin(0.2)).take(10).sum == sumTaylorSeries( taylorSeries( Sin(0.2) ), 10))
  }

  test("pow is correct") {

    assert(pow(-1, 0) == 1)
    assert(pow(-1, 1) == -1)
    assert(pow(-1, 2) == 1)
    assert(pow(-1, 3) == -1)
  }
}




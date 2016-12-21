/**
  * Created by kevin on 26/11/16.
  */
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import Mathz.{Radian, Sin}
import Mathz.Mathz._

@RunWith(classOf[JUnitRunner])
class TaylorSeriesSuite extends FunSuite {

  val eta = 0.001

  def ~=(x: BigDecimal, y: Double) = if ((x.toDouble - y).abs < eta) true else false

  def ~=(x: BigDecimal, y: BigDecimal) = if ((x.toDouble - y).abs < eta) true else false

  def ~~=(x: Seq[BigDecimal], y: Seq[Double]): Boolean = {
    x.zip(y).forall(tup => ~=(tup._1, tup._2))
  }


  test("factorial is correct about zero") {

    assert(factorial(0) == 1)
    assert(factorial(3) == 6)
    assert(factorial(4) == 24)
    assert(factorial(5) == 120)
    assert(factorial(6) == 720)
  }

  /* test("TaylorPolynomial is correct about zero") {
     val rad30 = Radian(30)
     assert(Sin(30).series(0).take(10).sum == Sin(30).series(0), 10)
   }*/

  test("pow is correct") {

    assert(pow(-1, 0) == 1)
    assert(pow(-1, 1) == -1)
    assert(pow(-1, 2) == 1)
    assert(pow(-1, 3) == -1)
  }


  test("Radian class is correct") {
    import Mathz.Radian

    val rad120 = Radian(120)
    val rad90 = Radian(90)
    val rad30 = Radian(30)

    assert(~=(Radian(180).degrees, 180))
    assert(~=(Radian(180).value, Math.PI))
    assert(~=(Radian(90).value, 2 *Math.PI / 4))
    assert(~=((rad30 + rad30).value, (rad90 - rad30).value))
    assert(~=((rad30 * 2).value, (rad120 / 2).value))
    assert(~=(rad30.degrees, 30))


  }

  test("squeeze for Pos is correct") {

    println("=>"+sin(Radian(1)))
    println("->"+Sin(Radian(1)).squeeze())
    assert(~=(Sin(Radian(1)).squeeze(), 1))
    assert(~=(Sin(Radian(89)).squeeze(), 89))
    assert(~=(Sin(Radian(91)).squeeze(), 89))
    assert(~=(Sin(Radian(179)).squeeze(), 1))
    assert(~=(Sin(Radian(181)).squeeze(), -1))
    assert(~=(Sin(Radian(269)).squeeze(), -89))
    assert(~=(Sin(Radian(271)).squeeze(), -89))
    assert(~=(Sin(Radian(359)).squeeze(), -1))
    assert(~=(Sin(Radian(449)).squeeze(), 89))
    assert(~=(Sin(Radian(539)).squeeze(), 1))
  }

  test("squeeze for Neg is correct") {
    assert(~=(Sin(Radian(-1)).squeeze(),  -1))
    assert(~=(Sin(Radian(-89)).squeeze(), -89))
    assert(~=(Sin(Radian(-91)).squeeze(), -89))
    assert(~=(Sin(Radian(-179)).squeeze(), -1))
    assert(~=(Sin(Radian(-181)).squeeze(), 1))
    assert(~=(Sin(Radian(-269)).squeeze(), 89))
    assert(~=(Sin(Radian(-271)).squeeze(), 89))
    assert(~=(Sin(Radian(-359)).squeeze(), 1))
    assert(~=(Sin(Radian(-449)).squeeze(), -89))
    assert(~=(Sin(Radian(-539)).squeeze(), -1))
  }

  //====================SINE================================================

  test("Sin (0 - -45) degrees should be positive and correct")  {
    val quad_1a = (0 to (-45, -1)).map(d => sin(Radian(d)))
    val quad_1b = (0 to (-45, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-46 - -90) degrees should be positive and correct")  {
    val quad_1a = (-46 to (-90, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-46 to (-90, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-91 - -135) degrees should be positive and correct")  {
    val quad_1a = (-91 to (-135, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-91 to (-135, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-136 - -180) degrees should be positive and correct")  {
    val quad_1a = (-136 to (-180, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-136 to (-180, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-181 - -225) degrees should be positive and correct")  {
    val quad_1a = (-181 to (-225, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-181 to (-225, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-226 - -270) degrees should be positive and correct")  {
    val quad_1a = (-226 to (-270, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-226 to (-270, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-270 - -315) degrees should be positive and correct")  {
    val quad_1a = (-270 to (-315, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-270 to (-315, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-316 - -360) degrees should be positive and correct")  {
    val quad_1a = (-316 to (-360, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-316 to (-360, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-361 - -405) degrees should be positive and correct")  {
    val quad_1a = (-361 to (-405, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-361 to (-405, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (-406 - -450) degrees should be positive and correct")  {
    val quad_1a = (-406 to (-450, -1)).map(d => sin(Radian(d)))
    val quad_1b = (-406 to (-450, -1)).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }


  test("Sin (0 - 45) degrees should be positive and correct")  {
    val quad_1a = (0 to 45).map(d => sin(Radian(d)))
    val quad_1b = (0 to 45).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (46 - 90) degrees should be positive and correct")  {
    val quad_1a = (46 to 90).map(d => sin(Radian(d)))
    val quad_1b = (46 to 90).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (91 - 135) degrees should be positive and correct")  {
    val quad_1a = (91 to 135).map(d => sin(Radian(d)))
    val quad_1b = (91 to 135).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (136 - 180) degrees should be positive and correct")  {
    val quad_1a = (136 to 180).map(d => sin(Radian(d)))
    val quad_1b = (136 to 180).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (181 - 225) degrees should be positive and correct")  {
    val quad_1a = (181 to 225).map(d => sin(Radian(d)))
    val quad_1b = (181 to 225).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (226 - 270) degrees should be positive and correct")  {
    val quad_1a = (226 to 270).map(d => sin(Radian(d)))
    val quad_1b = (226 to 270).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (270 - 315) degrees should be positive and correct")  {
    val quad_1a = (270 to 315).map(d => sin(Radian(d)))
    val quad_1b = (270 to 315).map(d => Math.sin(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (316 - 360) degrees should be positive and correct")  {
    val quad_1a = (316 to 360).map(d => sin(Radian(d)))
    val quad_1b = (316 to 360).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (361 - 405) degrees should be positive and correct")  {
    val quad_1a = (361 to 405).map(d => sin(Radian(d)))
    val quad_1b = (361 to 405).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("Sin (406 - 450) degrees should be positive and correct")  {
    val quad_1a = (406 to 450).map(d => sin(Radian(d)))
    val quad_1b = (406 to 450).map(d => Math.sin(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }



  //=====================COSINE=======================================

  test("cos (0 - -45) degrees should be positive and correct")  {
    val quad_1a = (0 to (-45, -1)).map(d => cos(Radian(d)))
    val quad_1b = (0 to (-45, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-46 - -90) degrees should be positive and correct")  {
    val quad_1a = (-46 to (-90, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-46 to (-90, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-91 - -135) degrees should be positive and correct")  {
    val quad_1a = (-91 to (-135, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-91 to (-135, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-136 - -180) degrees should be positive and correct")  {
    val quad_1a = (-136 to (-180, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-136 to (-180, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-181 - -225) degrees should be positive and correct")  {
    val quad_1a = (-181 to (-225, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-181 to (-225, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-226 - -270) degrees should be positive and correct")  {
    val quad_1a = (-226 to (-270, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-226 to (-270, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-270 - -315) degrees should be positive and correct")  {
    val quad_1a = (-270 to (-315, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-270 to (-315, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-316 - -360) degrees should be positive and correct")  {
    val quad_1a = (-316 to (-360, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-316 to (-360, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-361 - -405) degrees should be positive and correct")  {
    val quad_1a = (-361 to (-405, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-361 to (-405, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (-406 - -450) degrees should be positive and correct")  {
    val quad_1a = (-406 to (-450, -1)).map(d => cos(Radian(d)))
    val quad_1b = (-406 to (-450, -1)).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }


  test("cos (0 - 45) degrees should be positive and correct")  {
    val quad_1a = (0 to 45).map(d => cos(Radian(d)))
    val quad_1b = (0 to 45).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (46 - 90) degrees should be positive and correct")  {
    val quad_1a = (46 to 90).map(d => cos(Radian(d)))
    val quad_1b = (46 to 90).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (91 - 135) degrees should be positive and correct")  {
    val quad_1a = (91 to 135).map(d => cos(Radian(d)))
    val quad_1b = (91 to 135).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (136 - 180) degrees should be positive and correct")  {
    val quad_1a = (136 to 180).map(d => cos(Radian(d)))
    val quad_1b = (136 to 180).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (181 - 225) degrees should be positive and correct")  {
    val quad_1a = (181 to 225).map(d => cos(Radian(d)))
    val quad_1b = (181 to 225).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (226 - 270) degrees should be positive and correct")  {
    val quad_1a = (226 to 270).map(d => cos(Radian(d)))
    val quad_1b = (226 to 270).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (270 - 315) degrees should be positive and correct")  {
    val quad_1a = (270 to 315).map(d => cos(Radian(d)))
    val quad_1b = (270 to 315).map(d => Math.cos(Radian(d).value.toDouble))
    //quad_1a.zip(quad_1b).foreach(println(_))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (316 - 360) degrees should be positive and correct")  {
    val quad_1a = (316 to 360).map(d => cos(Radian(d)))
    val quad_1b = (316 to 360).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (361 - 405) degrees should be positive and correct")  {
    val quad_1a = (361 to 405).map(d => cos(Radian(d)))
    val quad_1b = (361 to 405).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }

  test("cos (406 - 450) degrees should be positive and correct")  {
    val quad_1a = (406 to 450).map(d => cos(Radian(d)))
    val quad_1b = (406 to 450).map(d => Math.cos(Radian(d).value.toDouble))
    //println(quad_1a.zip(quad_1b))
    assert(~~=(quad_1a, quad_1b))

  }



  test("Sin gives correct number of decimal places") {

    assert(~=(sin(Radian(10), 2).toString().split('.')(1).length, 2))
    assert(~=(sin(Radian(20), 3).toString().split('.')(1).length, 3))
    assert(~=(sin(Radian(30), 4).toString().split('.')(1).length, 4))
    assert(~=(sin(Radian(40), 5).toString().split('.')(1).length, 5))
    assert(~=(sin(Radian(10), 6).toString().split('.')(1).length, 6))
    assert(~=(sin(Radian(20), 7).toString().split('.')(1).length, 7))
    assert(~=(sin(Radian(30), 8).toString().split('.')(1).length, 8))
    assert(~=(sin(Radian(40), 9).toString().split('.')(1).length, 9))
  }



/*  test("E values are correct about zero") {

    assert(~=(e(Radian(0.2)), Math.exp(0.2)))
    assert(~=(e(Radian(0.3)), Math.exp(0.3)))
    assert(~=(e(Radian(0.4)), Math.exp(0.4)))
    assert(~=(e(Radian(0.5)), Math.exp(0.5)))
    assert(~=(e(Radian(0.6)), Math.exp(0.6)))
  }

  test("E values without mem are correct about zero") {

    assert(~=(e(Radian(0.2),10, mem = false), Math.exp(0.2)))
    assert(~=(e(Radian(0.3),10, mem = false), Math.exp(0.3)))
    assert(~=(e(Radian(0.4),10, mem = false), Math.exp(0.4)))
    assert(~=(e(Radian(0.5),10, mem = false), Math.exp(0.5)))
    assert(~=(e(Radian(0.6),10, mem = false), Math.exp(0.6)))
    assert(~=(e(Radian(1),  5,  mem = false), Math.exp(1)))
    assert(~=(e(Radian(1),  5,  mem = false), Math.exp(1)))
  }

  test("E values with mem are correct about zero") {

    assert(~=(e(Radian(0.2),10, mem = true), Math.exp(0.2)))
    assert(~=(e(Radian(0.3),10, mem = true), Math.exp(0.3)))
    assert(~=(e(Radian(0.4),10, mem = true), Math.exp(0.4)))
    assert(~=(e(Radian(0.5),10, mem = true), Math.exp(0.5)))
    assert(~=(e(Radian(0.6),10, mem = true), Math.exp(0.6)))
    assert(~=(e(Radian(1),  5,  mem = true), Math.exp(1)))
    assert(~=(e(Radian(1),  5,  mem = true), Math.exp(1)))

  }*/

}




package Mathz

class Radian private (val degrees: BigDecimal) {

  private val PI = Math.PI

  val value = degrees * PI / 180

  def +(other: Radian) = Radian(this.value + other.value)

  def -(other: Radian) = Radian(this.value - other.value)

  def *(by: Double) = Radian(this.value * by)

  def /(by: Double) = Radian(this.value / by)

  def %(other: Radian) = Radian(this.value / other.value)

  override def toString: String = "("+degrees + " * PI) / " + 180 + ")"

}

object Radian { def apply(degree: BigDecimal) = new Radian(degree) }

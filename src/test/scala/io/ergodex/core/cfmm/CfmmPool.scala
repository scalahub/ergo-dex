package io.ergodex
package core.cfmm

/*
  @feeNum is the numerator in the ratio of fee to be taken.
  @denom is the denominator in the ratio of fee to be taken.

  If fee is 5% then feeNum = 5 and denom = 100

 */
final case class Fee(feeNum: Int, denom: Int) {
  require(feeNum >= 0)
  require(feeNum < denom)

  // the numerator of the remaining part after fee is deducted (the actual amount used on computation)
  val nonFeeNum = denom - feeNum
}

final case class PoolConfig(fee: Fee, minInitialDeposit: Long)

final case class CfmmPool(x: Long, y: Long, supplyLP: Long, config: PoolConfig) {
  import config._
  import fee._

  require(x >= 0)
  require(y >= 0)
  require(supplyLP >= 0)

  def deposit(inX: Long, inY: Long): CfmmPool = {
    val unlocked = math.min(
      inX * supplyLP / x,
      inY * supplyLP / y
    )
    copy(x + inX, y + inY, supplyLP + unlocked)
  }

  def redeem(inLp: Long): CfmmPool = {
    require(inLp <= supplyLP)
    val redeemedX = inLp * x / supplyLP
    val redeemedY = inLp * y / supplyLP
    copy(x - redeemedX, y - redeemedY, supplyLP - inLp)
  }

  def inputAmount(token: String, output: Long): Long =
    if (token == "x" && outputAmount("x", output) > 0)
      (y * output * denom / ((x - output) * nonFeeNum)) + 1
    else if (token == "y" && outputAmount("y", output) > 0)
      (x * output * denom / ((y - output) * nonFeeNum)) + 1
    else -1L

  def outputAmount(token: String, input: Long): Long = {
    def out(in: Long, out: Long) =
      BigInt(out) * input * nonFeeNum /
      (in * 1000 + input * nonFeeNum)
    (if (token == "x") out(x, y) else out(y, x)).toLong
  }

  def swap(asset: String, in: Long): CfmmPool = {
    require(in > 0)
    val (deltaX, deltaY) =
      if (asset == "x") {
        (in, -y * in * nonFeeNum / (x * 1000 + in * nonFeeNum))
      } else
        (-x * in * nonFeeNum / (y * 1000 + in * nonFeeNum), in)
    copy(x + deltaX, y + deltaY)
  }

  def donateXY(deltaX: Long, deltaY: Long): CfmmPool =
    copy(x + deltaX, y + deltaY, supplyLP).ensuring(deltaX >= 0 && deltaY >= 0)

  def burn(lpToBurn: Long): CfmmPool = copy(x, y, supplyLP - lpToBurn).ensuring(lpToBurn > 0 && lpToBurn < supplyLP)
}

object CfmmPool {

  def init(inX: Long, inY: Long, config: PoolConfig): CfmmPool = {
    import config._
    require(inX >= minInitialDeposit && inY >= minInitialDeposit)
    val share = math.sqrt(inX * inY).toLong // todo: overflow
    CfmmPool(inX, inY, share, config)
  }
}

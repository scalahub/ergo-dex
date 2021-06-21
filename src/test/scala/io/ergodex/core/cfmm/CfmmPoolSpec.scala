package io.ergodex.core.cfmm

import io.ergodex.core.cfmm.generators._
import org.scalatest.flatspec._
import org.scalatest.matchers._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.util.UUID

class CfmmPoolSpec extends AnyFlatSpec with should.Matchers with ScalaCheckPropertyChecks {

  it should "support minimal amount swap [X]" in {
    forAll(cfmmConfGen) { conf =>
      forAll(initialDepositGen(conf.minInitialDeposit)) { case (ix, iy) =>
        whenever(ix >= conf.minInitialDeposit && iy >= conf.minInitialDeposit) {
          val pool0      = CfmmPool.init(ix, iy, conf)
          val minAmountX = pool0.inputAmount(token = "x", output = 1L)
          whenever(minAmountX > 0) {
            val pool1 = pool0.swap("x", minAmountX)
            pool1.x should be > pool0.x
            pool1.y should be < pool0.y
            if (conf.fee.feeNum > 0)
              pool0.x * pool0.y should be > pool1.x * pool1.y
            else
              pool0.x * pool0.y should be >= pool1.x * pool1.y
          }
        }
      }
    }
  }

  it should "support minimal amount swap [Y]" in {
    forAll(cfmmConfGen) { conf =>
      forAll(initialDepositGen(conf.minInitialDeposit)) { case (ix, iy) =>
        whenever(ix >= conf.minInitialDeposit && iy >= conf.minInitialDeposit) {
          val pool0      = CfmmPool.init(ix, iy, conf)
          val minAmountY = pool0.inputAmount(token = "y", output = 1L)
          whenever(minAmountY > 0) {
            val pool1 = pool0.swap("y", minAmountY)
            pool1.x should be < pool0.x
            pool1.y should be > pool0.y
            if (conf.fee.feeNum > 0)
              pool0.x * pool0.y should be > pool1.x * pool1.y
            else
              pool0.x * pool0.y should be >= pool1.x * pool1.y
          }
        }
      }
    }
  }

  it should "support full liquidation" in {
    forAll(cfmmConfGen) { conf =>
      forAll(initialDepositGen(conf.minInitialDeposit)) { case (ix, iy) =>
        whenever(ix >= conf.minInitialDeposit && iy >= conf.minInitialDeposit) {
          val pool0 = CfmmPool.init(ix, iy, conf)
          forAll(depositSwapGen) { actions =>
            val pool = actions.foldLeft(pool0) { case (pool, op) =>
              op match {
                case Deposit(x, y)       => pool.deposit(x, y)
                case Redeem(lp)          => pool.redeem(lp)
                case Swap(asset, amount) => pool.swap(asset, amount)
                case Burn(amount)        => pool.burn(amount)
              }
            }

            val poolDepleted = pool.redeem(pool.supplyLP)
            poolDepleted.x should be(0)
            poolDepleted.y should be(0)
            poolDepleted.supplyLP should be(0)
          }
        }
      }
    }
  }
}

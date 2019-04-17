package Discrete
import math._
object Arithmetic {
  def powMod (b: Long, pot: Long, mod: Long) : Long = {
    if (pot == 1) b % mod else {
      val pot2 = pot/2
      val pm1 = powMod (b, pot2, mod)
      val pm2 = powMod (b, pot-pot2, mod)
      val partial = ((pm1 % mod) * (pm2 % mod)).ensuring(res =>
        res > pm1 % mod && res > pm2 % mod, "Long overflow multiplying "+pm1+" by "+pm2)
      partial % mod
    }
  }

  def lg(x: Double):Double = log(x)/log(2)
}

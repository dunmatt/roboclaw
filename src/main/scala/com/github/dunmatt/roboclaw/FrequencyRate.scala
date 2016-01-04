package com.github.dunmatt.roboclaw

import squants._
import squants.time.{ Frequency, Hertz, Time }

// TODO: fork squants and add this there
final class FrequencyRate private(val value: Double, val unit: FrequencyRateUnit)
            extends Quantity[FrequencyRate] {
  def dimension = FrequencyRate

  def toHertzPerSecond = to(HertzPerSecond)
  def toKilohertzPerSecond = to(KilohertzPerSecond)
  def toMegahertzPerSecond = to(MegahertzPerSecond)
  def toGigahertzPerSecond = to(GigahertzPerSecond)
  def toTerahertzPerSecond = to(TerahertzPerSecond)

  def *(t: Time): Frequency = Hertz(toHertzPerSecond * t.toSeconds)
}

object FrequencyRate extends Dimension[FrequencyRate] {
  private[roboclaw] def apply[A](n: A, unit: FrequencyRateUnit)(implicit num: Numeric[A]) = new FrequencyRate(num.toDouble(n), unit)
  def apply = parse _
  def name = "Frequency Rate"
  def primaryUnit = HertzPerSecond
  def siUnit = HertzPerSecond
  def units = Set(HertzPerSecond, KilohertzPerSecond, MegahertzPerSecond, GigahertzPerSecond, TerahertzPerSecond)
}

trait FrequencyRateUnit extends UnitOfMeasure[FrequencyRate] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = FrequencyRate(n, this)
}

object HertzPerSecond extends FrequencyRateUnit with PrimaryUnit with SiUnit {
  val symbol = "Hz/s"
}

object KilohertzPerSecond extends FrequencyRateUnit {
  val symbol = "kHz/s"
  val conversionFactor = MetricSystem.Kilo
}

object MegahertzPerSecond extends FrequencyRateUnit {
  val symbol = "MHz/s"
  val conversionFactor = MetricSystem.Mega
}

object GigahertzPerSecond extends FrequencyRateUnit {
  val symbol = "GHz/s"
  val conversionFactor = MetricSystem.Giga
}

object TerahertzPerSecond extends FrequencyRateUnit {
  val symbol = "THz/s"
  val conversionFactor = MetricSystem.Tera
}

object FrequencyRateConversions {
  implicit class FrequencyRateConversions[A](n: A)(implicit num: Numeric[A]) {
    def hertzPerSecond = HertzPerSecond(n)
    def kilohertzPerSecond = KilohertzPerSecond(n)
    def megahertzPerSecond = MegahertzPerSecond(n)
    def gigahertzPerSecond = GigahertzPerSecond(n)
    def terahertzPerSecond = TerahertzPerSecond(n)
  }

  implicit class RateableFrequency(f: Frequency) {
    def /(t: Time): FrequencyRate = HertzPerSecond(f.toHertz / t.toSeconds)
  }

  implicit object FrequencyRateNumeric extends AbstractQuantityNumeric[FrequencyRate](FrequencyRate.primaryUnit)
}



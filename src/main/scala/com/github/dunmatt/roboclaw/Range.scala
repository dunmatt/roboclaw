package com.github.dunmatt.roboclaw

import squants.Quantity

trait Range[D] {
  def min: D
  def max: D
  def contains(q: D): Boolean
  def percentageThrough(q: D): Double
}

case class IntRange(min: Int, max: Int) extends Range[Int] {
  def contains(q: Int): Boolean = min <= q && q <= max
  def percentageThrough(q: Int): Double = (q - min) / (max - min)
}

case class QuantityRange[D <: Quantity[D]](min: D, max: D) extends Range[D] {
  def contains(q: D): Boolean = min <= q && q <= max
  def percentageThrough(q: D): Double = (q - min) / (max - min)
}

object Range {
  def apply(min: Int, max: Int): Range[Int] = IntRange(min, max)
  def apply[D <: Quantity[D]](min: D, max: D): Range[D] = QuantityRange(min, max)
}

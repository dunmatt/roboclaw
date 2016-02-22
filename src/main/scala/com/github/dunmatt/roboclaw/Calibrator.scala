package com.github.dunmatt.roboclaw

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{ Failure, Try }

object Calibrator {
  def calibrate(addr: Byte, channel1: Boolean, port: CommLayer): Future[Unit] = {
    calibrateVelocity(addr, channel1, port).flatMap { _ =>
      calibratePosition(addr, channel1, port)
    }
  }

  def calibrateVelocity(addr: Byte, channel1: Boolean, port: CommLayer): Future[Unit] = null // TODO: write me!

  def calibratePosition(addr: Byte, channel1: Boolean, port: CommLayer): Future[Unit] = null // TODO: write me!
}

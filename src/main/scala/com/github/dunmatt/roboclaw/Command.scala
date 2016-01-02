package com.github.dunmatt.roboclaw

import scala.concurrent.Promise

trait Command {
  type ResponseType
  val resultsPromise = Promise[ResponseType]
  val results = resultsPromise.future
  def bytes: Seq[Byte]
  def parseResults(data: Seq[Byte]): ResponseType
}

// COMPATIBILITY COMMANDS
case class DriveForwardM1(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x00, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveBackwardsM1(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x01, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveForwardM2(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x04, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveBackwardsM2(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x05, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveM1(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x06, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveM2(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x07, speed))
  def parseResults(data: Seq[Byte]) = Unit
}


// MIXED MODE COMPATIBILITY COMMANDS

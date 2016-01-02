package com.github.dunmatt.roboclaw

import scala.concurrent.Promise
import squants.electro.ElectricPotential
import squants.electro.ElectricPotentialConversions._

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


// MIXED MODE (ie diff dive) COMPATIBILITY COMMANDS
// NOTE: these commands only work after the controller has recieved one linear and
//       one rotational command, make sure to chain them up
case class DriveForward(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x08, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveBackwards(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x09, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class TurnRight(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x0A, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class TurnLeft(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x0B, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class DriveForwardOrBackward(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x0C, speed))
  def parseResults(data: Seq[Byte]) = Unit
}

case class TurnLeftOrRight(address: Byte, speed: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x0D, speed))
  def parseResults(data: Seq[Byte]) = Unit
}


// VERSION, STATUS, AND SETTINGS COMMANDS
case class ReadFirmwareVersion(address: Byte) extends Command {
  type ResponseType = String
  def bytes = Seq(address, 0x15)
  def parseResults(data: Seq[Byte]) = {
    new String(data.slice(0, data.indexOfSlice(Seq(0x0A, 0x00))).toArray)
  }
}

case class ReadMainBatteryVoltage(address: Byte) extends Command {
  type ResponseType = ElectricPotential
  def bytes = Seq(address, 0x18)
  def parseResults(data: Seq[Byte]) = {
    Utilities.readSignedShort(data).volts / 10d
  }
}

case class ReadLogicBatteryVoltage(address: Byte) extends Command {
  type ResponseType = ElectricPotential
  def bytes = Seq(address, 0x19)
  def parseResults(data: Seq[Byte]) = {
    Utilities.readSignedShort(data).volts / 10d
  }
}



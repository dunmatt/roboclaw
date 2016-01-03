package com.github.dunmatt.roboclaw

import scala.concurrent.Promise
import squants.electro.{ ElectricCurrent, ElectricPotential }
import squants.electro.ElectricCurrentConversions._
import squants.electro.ElectricPotentialConversions._
import squants.thermal.Temperature
import squants.thermal.TemperatureConversions._

sealed trait Command {
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

case class ReadMotorPwmValues(address: Byte) extends Command {
  type ResponseType = TwoMotorData[Double]
  def bytes = Seq(address, 0x30)
  def parseResults(data: Seq[Byte]) = {
    TwoMotorData( Utilities.readSignedShort(data) / 32767d
                , Utilities.readSignedShort(data.drop(2)) / 32767d)
  }
}

case class ReadMotorCurrents(address: Byte) extends Command {
  type ResponseType = TwoMotorData[ElectricCurrent]
  def bytes = Seq(address, 0x31)
  def parseResults(data: Seq[Byte]) = {
    TwoMotorData( Utilities.readSignedShort(data).amps / 100d
                , Utilities.readSignedShort(data.drop(2)).amps / 100d)
  }
}

case class SetMainBatteryVoltages( address: Byte
                                 , min: ElectricPotential
                                 , max: ElectricPotential) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc( Seq[Byte](address, 0x39)
                               ++ Utilities.writeSignedShort(min.toVolts * 10)
                               ++ Utilities.writeSignedShort(max.toVolts * 10))
  def parseResults(data: Seq[Byte]) = Unit
}

case class SetLogicBatteryVoltages( address: Byte
                                  , min: ElectricPotential
                                  , max: ElectricPotential) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc( Seq[Byte](address, 0x3A)
                               ++ Utilities.writeSignedShort(min.toVolts * 10)
                               ++ Utilities.writeSignedShort(max.toVolts * 10))
  def parseResults(data: Seq[Byte]) = Unit
}

case class ReadMainBatteryVoltageSettings(address: Byte) extends Command {
  type ResponseType = Range[ElectricPotential]
  def bytes = Seq(address, 0x3B)
  def parseResults(data: Seq[Byte]) = {
    Range( Utilities.readSignedShort(data).volts / 10d
         , Utilities.readSignedShort(data.drop(2)).volts / 10d)
  }
}

case class ReadLogicBatteryVoltageSettings(address: Byte) extends Command {
  type ResponseType = Range[ElectricPotential]
  def bytes = Seq(address, 0x3C)
  def parseResults(data: Seq[Byte]) = {
    Range( Utilities.readSignedShort(data).volts / 10d
         , Utilities.readSignedShort(data.drop(2)).volts / 10d)
  }
}

case class SetS3S4S5Modes( address: Byte
                         , s3: Byte
                         , s4: Byte
                         , s5: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq[Byte](address, 0x4A, s3, s4, s5))
  def parseResults(data: Seq[Byte]) = Unit
}

case class GetS3S4S5Modes(address: Byte) extends Command {
  type ResponseType = (Byte, Byte, Byte)
  def bytes = Seq[Byte](address, 0x4B)
  def parseResults(data: Seq[Byte]) = (data(0), data(1), data(2))
}

object SPinModes {
  val DISABLED = 0x0.toByte
  val E_STOP_LATCHING = 0x1.toByte
  val E_STOP = 0x2.toByte
  val VOLTAGE_CLAMP = 0x3.toByte
  val M1_HOME = 0x4.toByte
  val M2_HOME = 0x4.toByte
}
  
case class FactoryReset(address: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Seq(address, 0x50)
  def parseResults(data: Seq[Byte]) = Unit
}

case class ReadPrimaryTemperature(address: Byte) extends Command {
  type ResponseType = Temperature
  def bytes = Seq(address, 0x52)
  def parseResults(data: Seq[Byte]) = Utilities.readSignedShort(data).celsius / 10d
}

case class ReadSecondaryTemperature(address: Byte) extends Command {
  type ResponseType = Temperature
  def bytes = Seq(address, 0x53)
  def parseResults(data: Seq[Byte]) = Utilities.readSignedShort(data).celsius / 10d
}

object StatusFlags {
  val NORMAL = 0x0000
  val M1_OVER_CURRENT_WARNING = 0x0001
  val M2_OVER_CURRENT_WARNING = 0x0002
  val E_STOP = 0x0004
  val TEMPERATURE_ERROR = 0x0008
  val TEMPERATURE_2_ERROR = 0x0010
  val MAIN_BATTERY_HIGH_ERROR = 0x0020
  val LOGIC_BATTERY_HIGH_ERROR = 0x0040
  val LOGIC_BATTERY_LOW_ERROR = 0x0080
  val M1_DRIVER_FAULT = 0x0100
  val M2_DRIVER_FAULT = 0x0200
  val MAIN_BATTERY_HIGH_WARNING = 0x0400
  val MAIN_BATTERY_LOW_WARNING = 0x0800
  val TEMPERATURE_WARNING = 0x1000
  val TEMPERATURE_2_WARNING = 0x2000
  val M1_HOME = 0X4000
  val M2_HOME = 0X8000
}

case class RoboclawStatus(status: Short) {
  import StatusFlags._
  def normal = status == NORMAL
  def m1OverCurrentWarning = (status & M1_OVER_CURRENT_WARNING) > 0
  def m2OverCurrentWarning = (status & M2_OVER_CURRENT_WARNING) > 0
  def eStop = (status & E_STOP) > 0
  def temperatureError = (status & TEMPERATURE_ERROR) > 0
  def temperature2Error = (status & TEMPERATURE_2_ERROR) > 0
  def mainBatteryHighError = (status & MAIN_BATTERY_HIGH_ERROR) > 0
  def logicBatteryHighError = (status & LOGIC_BATTERY_HIGH_ERROR) > 0
  def logicBatteryLowError = (status & LOGIC_BATTERY_LOW_ERROR) > 0
  def m1DriverFault = (status & M1_DRIVER_FAULT) > 0
  def m2DriverFault = (status & M2_DRIVER_FAULT) > 0
  def mainBatteryHighWarning = (status & MAIN_BATTERY_HIGH_WARNING) > 0
  def mainBatteryLowWarning = (status & MAIN_BATTERY_LOW_WARNING) > 0
  def temperatureWarning = (status & TEMPERATURE_WARNING) > 0
  def temperature2Warning = (status & TEMPERATURE_2_WARNING) > 0
  def m1Home = (status & M1_HOME) > 0
  def m2Home = (status & M2_HOME) > 0
}

case class ReadStatus(address: Byte) extends Command {
  type ResponseType = RoboclawStatus
  def bytes = Seq(address, 0x5A)
  def parseResults(data: Seq[Byte]) = RoboclawStatus(Utilities.readSignedShort(data))
}

sealed trait EncoderMode
case object QUADRATURE extends EncoderMode
case object ABSOLUTE extends EncoderMode

case class ReadEncoderMode(address: Byte) extends Command {
  type ResponseType = TwoMotorData[EncoderMode]
  def bytes = Seq(address, 0x5B)
  def parseResults(data: Seq[Byte]) = TwoMotorData( if((data(0) & 1) == 0) QUADRATURE else ABSOLUTE
                                                  , if((data(1) & 1) == 0) QUADRATURE else ABSOLUTE)
}

case class SetMotor1EncoderMode(address: Byte, mode: EncoderMode) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x5C, if(mode == ABSOLUTE) 1 else 0))
  def parseResults(data: Seq[Byte]) = Unit
}

case class SetMotor2EncoderMode(address: Byte, mode: EncoderMode) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x5D, if(mode == ABSOLUTE) 1 else 0))
  def parseResults(data: Seq[Byte]) = Unit
}

case class WriteSettingsToEeprom(address: Byte) extends Command {
  type ResponseType = Unit
  def bytes = Seq(address, 0x5E)
  def parseResults(data: Seq[Byte]) = Unit
}

case class ReadSettingsFromEeprom(address: Byte) extends Command {
  type ResponseType = TwoMotorData[EncoderMode]
  def bytes = Seq(address, 0x5F)
  def parseResults(data: Seq[Byte]) = TwoMotorData( if((data(0) & 1) == 0) QUADRATURE else ABSOLUTE
                                                  , if((data(1) & 1) == 0) QUADRATURE else ABSOLUTE)
}

case class SetM1CurrentLimit(address: Byte, max: ElectricCurrent) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc( Seq(address, 0x86.toByte)
                               ++ Utilities.writeSignedShort(max.toAmperes * 100)
                               ++ Seq[Byte](0, 0))
  def parseResults(data: Seq[Byte]) = Unit
}

case class SetM2CurrentLimit(address: Byte, max: ElectricCurrent) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc( Seq(address, 0x87.toByte)
                               ++ Utilities.writeSignedShort(max.toAmperes * 100)
                               ++ Seq[Byte](0, 0))
  def parseResults(data: Seq[Byte]) = Unit
}

case class ReadM1CurrentLimit(address: Byte) extends Command {
  type ResponseType = Range[ElectricCurrent]
  def bytes = Seq(address, 0x88.toByte)
  def parseResults(data: Seq[Byte]) = Range( Utilities.readSignedShort(data.drop(2)).amps / 100
                                           , Utilities.readSignedShort(data).amps / 100)
}

case class ReadM2CurrentLimit(address: Byte) extends Command {
  type ResponseType = Range[ElectricCurrent]
  def bytes = Seq(address, 0x89.toByte)
  def parseResults(data: Seq[Byte]) = Range( Utilities.readSignedShort(data.drop(2)).amps / 100
                                           , Utilities.readSignedShort(data).amps / 100)
}

sealed trait PwmMode
case object LOCKED_ANTIPHASE extends PwmMode
case object SIGN_MAGNITUDE extends PwmMode

case class SetPwmMode(address: Byte, mode: PwmMode) extends Command {
  type ResponseType = Unit
  def bytes = Utilities.withCrc(Seq(address, 0x94.toByte, if(mode == LOCKED_ANTIPHASE) 0x00 else 0x01))
  def parseResults(data: Seq[Byte]) = Unit
}

case class ReadPwmMode(address: Byte) extends Command {
  type ResponseType = PwmMode
  def bytes = Seq(address, 0x95.toByte)
  def parseResults(data: Seq[Byte]) = if (data(0) == 0) LOCKED_ANTIPHASE else SIGN_MAGNITUDE
}



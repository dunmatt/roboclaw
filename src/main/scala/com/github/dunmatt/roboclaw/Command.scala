package com.github.dunmatt.roboclaw

import java.nio.ByteBuffer
import scala.concurrent.Promise
import squants.electro.{ ElectricCurrent, ElectricPotential }
import squants.electro.ElectricCurrentConversions._
import squants.electro.ElectricPotentialConversions._
import squants.thermal.Temperature
import squants.thermal.TemperatureConversions._

sealed trait Command {
  type ResponseType
  protected[roboclaw] val resultsPromise = Promise[ResponseType]
  val results = resultsPromise.future

  def address: Byte
  def command: Byte
  def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    2
  }
  def parseResults(data: ByteBuffer): ResponseType

  def addCrc(buf: ByteBuffer, packetLenth: Int): Unit = {
    buf.putChar(packetLenth, Utilities.crc16(buf, packetLenth))
  }
}

sealed trait UnitCommand extends Command {
  type ResponseType = Unit
  def parseResults(data: ByteBuffer) = Unit  // TODO: barf everywhere if we don't get 0xff
}


// COMPATIBILITY COMMANDS
sealed trait SimpleMotorCommand extends UnitCommand {
  def speed: Byte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.put(2, speed)
    addCrc(buf, 3)
    5
  }
}

case class DriveForwardM1(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 0.toByte
}

case class DriveBackwardsM1(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 1.toByte
}

case class DriveForwardM2(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 4.toByte
}

case class DriveBackwardsM2(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 5.toByte
}

case class DriveM1(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 6.toByte
}

case class DriveM2(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 7.toByte
}


// MIXED MODE (ie diff dive) COMPATIBILITY COMMANDS
// NOTE: these commands only work after the controller has recieved one linear and
//       one rotational command, make sure to chain them up
case class DriveForward(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 8.toByte
}

case class DriveBackwards(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 9.toByte
}

case class TurnRight(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 10.toByte
}

case class TurnLeft(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 11.toByte
}

case class DriveForwardOrBackward(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 12.toByte
}

case class TurnLeftOrRight(address: Byte, speed: Byte) extends SimpleMotorCommand {
  val command = 13.toByte
}


// VERSION, STATUS, AND SETTINGS COMMANDS
case class ReadFirmwareVersion(address: Byte) extends Command {
  type ResponseType = String
  val command = 21.toByte
  def parseResults(data: ByteBuffer) = {
    val arr = data.array
    new String(arr.slice(data.arrayOffset, arr.indexOfSlice(Seq(0x0A, 0x00))))
  }
}

case class ReadMainBatteryVoltage(address: Byte) extends Command {
  type ResponseType = ElectricPotential
  val command = 24.toByte
  def parseResults(data: ByteBuffer) = data.getShort(0).volts / 10d
}

case class ReadLogicBatteryVoltage(address: Byte) extends Command {
  type ResponseType = ElectricPotential
  val command = 25.toByte
  def parseResults(data: ByteBuffer) = data.getShort(0).volts / 10d
}

case class ReadMotorPwmValues(address: Byte) extends Command {
  type ResponseType = TwoMotorData[Double]
  val command = 48.toByte
  def parseResults(data: ByteBuffer) = {
    TwoMotorData( data.getShort(0) / 32767d
                , data.getShort(2) / 32767d)
  }
}

case class ReadMotorCurrents(address: Byte) extends Command {
  type ResponseType = TwoMotorData[ElectricCurrent]
  val command = 49.toByte
  def parseResults(data: ByteBuffer) = {
    TwoMotorData( data.getShort(0).amps / 100d
                , data.getShort(2).amps / 100d)
  }
}

case class SetMainBatteryVoltages( address: Byte
                                 , min: ElectricPotential
                                 , max: ElectricPotential) extends UnitCommand {
  val command = 57.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.putShort(2, (min.toVolts * 10).toShort)
    buf.putShort(4, (max.toVolts * 10).toShort)
    addCrc(buf, 6)
    8
  }
}

case class SetLogicBatteryVoltages( address: Byte
                                  , min: ElectricPotential
                                  , max: ElectricPotential) extends UnitCommand {
  val command = 58.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.putShort(2, (min.toVolts * 10).toShort)
    buf.putShort(4, (max.toVolts * 10).toShort)
    addCrc(buf, 6)
    8
  }
}

case class ReadMainBatteryVoltageSettings(address: Byte) extends Command {
  type ResponseType = Range[ElectricPotential]
  val command = 59.toByte
  def parseResults(data: ByteBuffer) = {
    Range( data.getShort(0).volts / 10d
         , data.getShort(2).volts / 10d)
  }
}

case class ReadLogicBatteryVoltageSettings(address: Byte) extends Command {
  type ResponseType = Range[ElectricPotential]
  val command = 60.toByte
  def parseResults(data: ByteBuffer) = {
    Range( data.getShort(0).volts / 10d
         , data.getShort(2).volts / 10d)
  }
}

case class SetS3S4S5Modes( address: Byte
                         , s3: Byte
                         , s4: Byte
                         , s5: Byte) extends UnitCommand {
  val command = 74.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.put(2, s3)
    buf.put(3, s4)
    buf.put(4, s5)
    addCrc(buf, 5)
    7
  }
}

case class GetS3S4S5Modes(address: Byte) extends Command {
  type ResponseType = (Byte, Byte, Byte)
  val command = 75.toByte
  def parseResults(data: ByteBuffer) = (data.get(0), data.get(1), data.get(2))
}

object SPinModes {
  val DISABLED        = 0x0.toByte
  val E_STOP_LATCHING = 0x1.toByte
  val E_STOP          = 0x2.toByte
  val VOLTAGE_CLAMP   = 0x3.toByte
  val M1_HOME         = 0x4.toByte
  val M2_HOME         = 0x4.toByte
}
  
case class FactoryReset(address: Byte) extends UnitCommand {
  val command = 80.toByte
}

case class ReadPrimaryTemperature(address: Byte) extends Command {
  type ResponseType = Temperature
  val command = 82.toByte
  def parseResults(data: ByteBuffer) = data.getShort(0).celsius / 10d
}

case class ReadSecondaryTemperature(address: Byte) extends Command {
  type ResponseType = Temperature
  val command = 83.toByte
  def parseResults(data: ByteBuffer) = data.getShort(0).celsius / 10d
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
  val command = 90.toByte
  def parseResults(data: ByteBuffer) = RoboclawStatus(data.getShort(0))
}

sealed trait EncoderMode
case object QUADRATURE extends EncoderMode
case object ABSOLUTE extends EncoderMode

case class ReadEncoderMode(address: Byte) extends Command {
  type ResponseType = TwoMotorData[EncoderMode]
  val command = 91.toByte
  def parseResults(data: ByteBuffer) = TwoMotorData( if((data.get(0) & 1) == 0) QUADRATURE else ABSOLUTE
                                                   , if((data.get(1) & 1) == 0) QUADRATURE else ABSOLUTE)
}

case class SetMotor1EncoderMode(address: Byte, mode: EncoderMode) extends UnitCommand {
  val command = 92.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.put(2, if(mode == ABSOLUTE) 1 else 0)
    addCrc(buf, 3)
    5
  }
}

case class SetMotor2EncoderMode(address: Byte, mode: EncoderMode) extends UnitCommand {
  val command = 93.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.put(2, if(mode == ABSOLUTE) 1 else 0)
    addCrc(buf, 3)
    5
  }
}

case class WriteSettingsToEeprom(address: Byte) extends UnitCommand {
  val command = 94.toByte
}

case class ReadSettingsFromEeprom(address: Byte) extends Command {
  type ResponseType = TwoMotorData[EncoderMode]
  val command = 95.toByte
  def parseResults(data: ByteBuffer) = TwoMotorData( if((data.get(0) & 1) == 0) QUADRATURE else ABSOLUTE
                                                   , if((data.get(1) & 1) == 0) QUADRATURE else ABSOLUTE)
}

case class SetM1CurrentLimit(address: Byte, max: ElectricCurrent) extends UnitCommand {
  val command = 134.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.putShort(2, (max.toAmperes * 100).toShort)
    buf.putShort(4, 0)
    addCrc(buf, 6)
    8
  }
}

case class SetM2CurrentLimit(address: Byte, max: ElectricCurrent) extends UnitCommand {
  val command = 135.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.putShort(2, (max.toAmperes * 100).toShort)
    buf.putShort(4, 0)
    addCrc(buf, 6)
    8
  }
}

case class ReadM1CurrentLimit(address: Byte) extends Command {
  type ResponseType = Range[ElectricCurrent]
  val command = 136.toByte
  def parseResults(data: ByteBuffer) = Range( data.getShort(2).amps / 100
                                            , data.getShort(0).amps / 100)
}

case class ReadM2CurrentLimit(address: Byte) extends Command {
  type ResponseType = Range[ElectricCurrent]
  val command = 137.toByte
  def parseResults(data: ByteBuffer) = Range( data.getShort(2).amps / 100
                                            , data.getShort(0).amps / 100)
}

sealed trait PwmMode
case object LOCKED_ANTIPHASE extends PwmMode
case object SIGN_MAGNITUDE extends PwmMode

case class SetPwmMode(address: Byte, mode: PwmMode) extends UnitCommand {
  val command = 148.toByte
  override def populateByteBuffer(buf: ByteBuffer): Int = {
    buf.put(0, address)
    buf.put(1, command)
    buf.put(2, if(mode == LOCKED_ANTIPHASE) 0 else 1)
    addCrc(buf, 3)
    8
  }
}

case class ReadPwmMode(address: Byte) extends Command {
  type ResponseType = PwmMode
  val command = 149.toByte
  def parseResults(data: ByteBuffer) = if (data.get(0) == 0) LOCKED_ANTIPHASE else SIGN_MAGNITUDE
}


// ENCODER COMMANDS
case class EncoderStatus(status: Byte) {
  def underflow: Boolean = (status & 1) != 0
  def overflow: Boolean = (status & 4) != 0
  def forward: Boolean = (status & 2) != 0
}

case class ReadM1Encoder(address: Byte) extends Command {
  type ResponseType = (Long, EncoderStatus)
  val command = 16.toByte
  def parseResults(data: ByteBuffer) = {
    (data.getInt(0).toLong + Int.MaxValue, EncoderStatus(data.get(4)))
  }
}





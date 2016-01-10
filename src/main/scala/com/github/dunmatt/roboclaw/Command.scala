package com.github.dunmatt.roboclaw

import java.nio.ByteBuffer
import scala.util.{ Failure, Success, Try }
import squants.electro.{ ElectricCurrent, ElectricPotential }
import squants.electro.ElectricCurrentConversions._
import squants.electro.ElectricPotentialConversions._
import squants.thermal.Temperature
import squants.thermal.TemperatureConversions._
import squants.time.Frequency
import squants.time.FrequencyConversions._

sealed trait Command[ResponseType] {
  def address: Byte
  def command: Byte
  def populateByteBuffer(buf: ByteBuffer): Unit = {
    buf.clear
    buf.put(address)
    buf.put(command)
    buf.flip
  }
  def parseResults(data: ByteBuffer): Try[ResponseType]

  protected def addCrc(buf: ByteBuffer): Unit = {
    buf.putChar(Utilities.crc16(buf))
  }

  def expectsCrc: Boolean = true
}

sealed trait UnitCommand extends Command[Unit] {
  def parseResults(data: ByteBuffer) = {
    if (data.get(0) == 0xff.toByte) {
      Success(Unit)
    } else {
      Failure(new Exception(s"Invalid result ${data.get(0).toInt}, expected 0xff."))
    }
  }

  override def expectsCrc = false
}

sealed trait CrcCommand extends UnitCommand {
  def populateBufferMiddle(buf: ByteBuffer): Unit = Unit
  final override def populateByteBuffer(buf: ByteBuffer): Unit = {
    buf.clear
    buf.put(address)
    buf.put(command)
    populateBufferMiddle(buf)
    addCrc(buf)
    buf.flip
  }
}


// COMPATIBILITY COMMANDS
sealed trait SimpleMotorCommand extends CrcCommand {
  def speed: Byte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.put(speed)
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
case class ReadFirmwareVersion(address: Byte) extends Command[String] {
  val command = 21.toByte
  def parseResults(data: ByteBuffer) = {
    val arr = data.array
    Try(new String(arr.slice(data.arrayOffset, arr.indexOfSlice(Seq(0x0A, 0x00)))))
  }
}

case class ReadMainBatteryVoltage(address: Byte) extends Command[ElectricPotential] {
  val command = 24.toByte
  def parseResults(data: ByteBuffer) = Try(data.getShort.volts / 10d)
}

case class ReadLogicBatteryVoltage(address: Byte) extends Command[ElectricPotential] {
  val command = 25.toByte
  def parseResults(data: ByteBuffer) = Try(data.getShort.volts / 10d)
}

case class ReadMotorPwmValues(address: Byte) extends Command[TwoMotorData[Double]] {
  val command = 48.toByte
  def parseResults(data: ByteBuffer) = {
    Try(TwoMotorData( data.getShort / 32767d
                    , data.getShort / 32767d))
  }
}

case class ReadMotorCurrents(address: Byte) extends Command[TwoMotorData[ElectricCurrent]] {
  val command = 49.toByte
  def parseResults(data: ByteBuffer) = {
    Try(TwoMotorData( data.getShort.amps / 100d
                    , data.getShort.amps / 100d))
  }
}

case class SetMainBatteryVoltages( address: Byte
                                 , min: ElectricPotential
                                 , max: ElectricPotential) extends CrcCommand {
  val command = 57.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((min.toVolts * 10).toShort)
    buf.putShort((max.toVolts * 10).toShort)
  }
}

case class SetLogicBatteryVoltages( address: Byte
                                  , min: ElectricPotential
                                  , max: ElectricPotential) extends CrcCommand {
  val command = 58.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((min.toVolts * 10).toShort)
    buf.putShort((max.toVolts * 10).toShort)
  }
}

case class ReadMainBatteryVoltageSettings(address: Byte) extends Command[Range[ElectricPotential]] {
  val command = 59.toByte
  def parseResults(data: ByteBuffer) = {
    Try(Range( data.getShort.volts / 10d
             , data.getShort.volts / 10d))
  }
}

case class ReadLogicBatteryVoltageSettings(address: Byte) extends Command[Range[ElectricPotential]] {
  val command = 60.toByte
  def parseResults(data: ByteBuffer) = {
    Try(Range( data.getShort.volts / 10d
             , data.getShort.volts / 10d))
  }
}

case class SetS3S4S5Modes( address: Byte
                         , s3: Byte
                         , s4: Byte
                         , s5: Byte) extends CrcCommand {
  val command = 74.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.put(s3)
    buf.put(s4)
    buf.put(s5)
  }
}

case class GetS3S4S5Modes(address: Byte) extends Command[(Byte, Byte, Byte)] {
  val command = 75.toByte
  def parseResults(data: ByteBuffer) = Try((data.get, data.get, data.get))
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

case class ReadPrimaryTemperature(address: Byte) extends Command[Temperature] {
  val command = 82.toByte
  def parseResults(data: ByteBuffer) = Try(data.getShort.celsius / 10d)
}

case class ReadSecondaryTemperature(address: Byte) extends Command[Temperature] {
  val command = 83.toByte
  def parseResults(data: ByteBuffer) = Try(data.getShort.celsius / 10d)
}

object StatusFlags {
  val NORMAL                    = 0x0000
  val M1_OVER_CURRENT_WARNING   = 0x0001
  val M2_OVER_CURRENT_WARNING   = 0x0002
  val E_STOP                    = 0x0004
  val TEMPERATURE_ERROR         = 0x0008
  val TEMPERATURE_2_ERROR       = 0x0010
  val MAIN_BATTERY_HIGH_ERROR   = 0x0020
  val LOGIC_BATTERY_HIGH_ERROR  = 0x0040
  val LOGIC_BATTERY_LOW_ERROR   = 0x0080
  val M1_DRIVER_FAULT           = 0x0100
  val M2_DRIVER_FAULT           = 0x0200
  val MAIN_BATTERY_HIGH_WARNING = 0x0400
  val MAIN_BATTERY_LOW_WARNING  = 0x0800
  val TEMPERATURE_WARNING       = 0x1000
  val TEMPERATURE_2_WARNING     = 0x2000
  val M1_HOME                   = 0x4000
  val M2_HOME                   = 0x8000
}

case class RoboclawStatus(status: Short) {
  import StatusFlags._
  def normal = status == NORMAL
  def m1OverCurrentWarning   = (status & M1_OVER_CURRENT_WARNING)   != 0
  def m2OverCurrentWarning   = (status & M2_OVER_CURRENT_WARNING)   != 0
  def eStop                  = (status & E_STOP)                    != 0
  def temperatureError       = (status & TEMPERATURE_ERROR)         != 0
  def temperature2Error      = (status & TEMPERATURE_2_ERROR)       != 0
  def mainBatteryHighError   = (status & MAIN_BATTERY_HIGH_ERROR)   != 0
  def logicBatteryHighError  = (status & LOGIC_BATTERY_HIGH_ERROR)  != 0
  def logicBatteryLowError   = (status & LOGIC_BATTERY_LOW_ERROR)   != 0
  def m1DriverFault          = (status & M1_DRIVER_FAULT)           != 0
  def m2DriverFault          = (status & M2_DRIVER_FAULT)           != 0
  def mainBatteryHighWarning = (status & MAIN_BATTERY_HIGH_WARNING) != 0
  def mainBatteryLowWarning  = (status & MAIN_BATTERY_LOW_WARNING)  != 0
  def temperatureWarning     = (status & TEMPERATURE_WARNING)       != 0
  def temperature2Warning    = (status & TEMPERATURE_2_WARNING)     != 0
  def m1Home                 = (status & M1_HOME)                   != 0
  def m2Home                 = (status & M2_HOME)                   != 0
}

case class ReadStatus(address: Byte) extends Command[RoboclawStatus] {
  val command = 90.toByte
  def parseResults(data: ByteBuffer) = Try(RoboclawStatus(data.getShort))
}

sealed trait EncoderMode
case object QUADRATURE extends EncoderMode
case object ABSOLUTE extends EncoderMode

case class ReadEncoderMode(address: Byte) extends Command[TwoMotorData[EncoderMode]] {
  val command = 91.toByte
  def parseResults(data: ByteBuffer) = Try(TwoMotorData( if((data.get & 1) == 0) QUADRATURE else ABSOLUTE
                                                       , if((data.get & 1) == 0) QUADRATURE else ABSOLUTE))
}

case class SetMotor1EncoderMode(address: Byte, mode: EncoderMode) extends CrcCommand {
  val command = 92.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.put(if(mode == ABSOLUTE) 1.toByte else 0.toByte)
  }
}

case class SetMotor2EncoderMode(address: Byte, mode: EncoderMode) extends CrcCommand {
  val command = 93.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.put(if(mode == ABSOLUTE) 1.toByte else 0.toByte)
  }
}

case class WriteSettingsToEeprom(address: Byte) extends UnitCommand {
  val command = 94.toByte
}

case class ReadSettingsFromEeprom(address: Byte) extends Command[TwoMotorData[EncoderMode]] {
  val command = 95.toByte
  def parseResults(data: ByteBuffer) = Try(TwoMotorData( if((data.get & 1) == 0) QUADRATURE else ABSOLUTE
                                                       , if((data.get & 1) == 0) QUADRATURE else ABSOLUTE))
}

case class SetM1CurrentLimit(address: Byte, max: ElectricCurrent) extends CrcCommand {
  val command = 134.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((max.toAmperes * 100).toShort)
    buf.putShort(0)
  }
}

case class SetM2CurrentLimit(address: Byte, max: ElectricCurrent) extends CrcCommand {
  val command = 135.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((max.toAmperes * 100).toShort)
    buf.putShort(0)
  }
}

case class ReadM1CurrentLimit(address: Byte) extends Command[Range[ElectricCurrent]] {
  val command = 136.toByte
  def parseResults(data: ByteBuffer) = {
    val max = data.getShort.amps
    Try(Range(data.getShort.amps / 100, max / 100))
  }
}

case class ReadM2CurrentLimit(address: Byte) extends Command[Range[ElectricCurrent]] {
  val command = 137.toByte
  def parseResults(data: ByteBuffer) = {
    val max = data.getShort.amps
    Try(Range(data.getShort.amps / 100, max / 100))
  }
}

sealed trait PwmMode
case object LOCKED_ANTIPHASE extends PwmMode
case object SIGN_MAGNITUDE extends PwmMode

case class SetPwmMode(address: Byte, mode: PwmMode) extends CrcCommand {
  val command = 148.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.put(if(mode == LOCKED_ANTIPHASE) 0.toByte else 1.toByte)
  }
}

case class ReadPwmMode(address: Byte) extends Command[PwmMode] {
  val command = 149.toByte
  def parseResults(data: ByteBuffer) = Try(if (data.get == 0) LOCKED_ANTIPHASE else SIGN_MAGNITUDE)
}


// ENCODER COMMANDS
case class EncoderStatus(status: Byte) {
  def underflow: Boolean = (status & 1) != 0
  def overflow: Boolean = (status & 4) != 0
  def forward: Boolean = (status & 2) != 0
}

case class ReadM1Encoder(address: Byte) extends Command[(Long, EncoderStatus)] {
  val command = 16.toByte
  def parseResults(data: ByteBuffer) = {
    // TODO: this is very likely wrong!
    Try((data.getInt.toLong + Int.MaxValue + 1, EncoderStatus(data.get)))
  }
}

case class ReadM2Encoder(address: Byte) extends Command[(Long, EncoderStatus)] {
  val command = 17.toByte
  def parseResults(data: ByteBuffer) = {
    // TODO: this is very likely wrong!
    Try((data.getInt.toLong + Int.MaxValue + 1, EncoderStatus(data.get)))
  }
}

case class ReadM1Speed(address: Byte) extends Command[(Frequency, EncoderStatus)] {
  val command = 18.toByte
  def parseResults(data: ByteBuffer) = {
    Try((data.getInt.hertz, EncoderStatus(data.get)))
  }
}

case class ReadM2Speed(address: Byte) extends Command[(Frequency, EncoderStatus)] {
  val command = 19.toByte
  def parseResults(data: ByteBuffer) = {
    Try((data.getInt.hertz, EncoderStatus(data.get)))
  }
}

case class ResetQuadratureEncoderCounters(address: Byte) extends CrcCommand {
  val command = 20.toByte
}

case class SetQuadratureEncoder1Value(address: Byte, value: Long) extends CrcCommand {
  val command = 22.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(value.toInt)
  }
}

case class SetQuadratureEncoder2Value(address: Byte, value: Long) extends CrcCommand {
  val command = 23.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(value.toInt)
  }
}


// ADVANCED MOTOR CONTROL
case class SetVelocityPidConstantsM1( address: Byte
                                    , qpps: Frequency
                                    , p: Int
                                    , i: Int
                                    , d: Int) extends CrcCommand {
  val command = 28.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(d)
    buf.putInt(p)
    buf.putInt(i)
    buf.putInt(qpps.toHertz.toInt)
  }
}

case class SetVelocityPidConstantsM2( address: Byte
                                    , qpps: Frequency
                                    , p: Int
                                    , i: Int
                                    , d: Int) extends CrcCommand {
  val command = 29.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(d)
    buf.putInt(p)
    buf.putInt(i)
    buf.putInt(qpps.toHertz.toInt)
  }
}

case class ReadRawSpeedM1(address: Byte) extends Command[(Frequency, Boolean)] {  // boolean here is "forward"
  val command = 30.toByte
  def parseResults(data: ByteBuffer) = {
    Try((data.getInt.hertz * 300, data.get == 0d))
  }
}

case class ReadRawSpeedM2(address: Byte) extends Command[(Frequency, Boolean)] {  // boolean here is "forward"
  val command = 31.toByte
  def parseResults(data: ByteBuffer) = {
    Try((data.getInt.hertz * 300, data.get == 0d))
  }
}

case class DriveM1WithSignedDutyCycle(address: Byte, dutyCycle: Double) extends CrcCommand {
  val command = 32.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycle * 32767).toShort)
  }
}

case class DriveM2WithSignedDutyCycle(address: Byte, dutyCycle: Double) extends CrcCommand {
  val command = 33.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycle * 32767).toShort)
  }
}

case class DriveM1M2WithSignedDutyCycle(address: Byte, dutyCycle: TwoMotorData[Double]) extends CrcCommand {
  val command = 34.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycle.m1 * 32767).toShort)
    buf.putShort((dutyCycle.m2 * 32767).toShort)
  }
}

case class DriveM1WithSignedSpeed(address: Byte, speed: Frequency) extends CrcCommand {
  val command = 35.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speed.toHertz.toInt)
  }
}

case class DriveM2WithSignedSpeed(address: Byte, speed: Frequency) extends CrcCommand {
  val command = 36.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speed.toHertz.toInt)
  }
}

case class DriveM1M2WithSignedSpeed(address: Byte, speeds: TwoMotorData[Frequency]) extends CrcCommand {
  val command = 37.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(speeds.m2.toHertz.toInt)
  }
}

case class DriveM1WithSignedSpeedAndAcceleration( address: Byte
                                                , speed: Frequency
                                                , accel: FrequencyRate) extends CrcCommand {
  val command = 38.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
  }
}

case class DriveM2WithSignedSpeedAndAcceleration( address: Byte
                                                , speed: Frequency
                                                , accel: FrequencyRate) extends CrcCommand {
  val command = 39.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
  }
}

case class DriveM1M2WithSignedSpeedAndAcceleration( address: Byte
                                                  , speeds: TwoMotorData[Frequency]
                                                  , accel: FrequencyRate) extends CrcCommand {
  val command = 40.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(speeds.m2.toHertz.toInt)
  }
}

case class BufferedM1DriveWithSignedSpeedAndDistance( address: Byte
                                                    , speed: Frequency
                                                    , distance: Int
                                                    , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 41.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(distance)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedM2DriveWithSignedSpeedAndDistance( address: Byte
                                                    , speed: Frequency
                                                    , distance: Int
                                                    , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 42.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(distance)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedM1M2DriveWithSignedSpeedAndDistance( address: Byte
                                                      , speeds: TwoMotorData[Frequency]
                                                      , distances: TwoMotorData[Int]
                                                      , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 43.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(distances.m1)
    buf.putInt(speeds.m2.toHertz.toInt)
    buf.putInt(distances.m2)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedM1DriveWithSignedSpeedAccelAndDistance( address: Byte
                                                         , distance: Int
                                                         , speed: Frequency
                                                         , accel: FrequencyRate
                                                         , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 44.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(distance)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedM2DriveWithSignedSpeedAccelAndDistance( address: Byte
                                                         , distance: Int
                                                         , speed: Frequency
                                                         , accel: FrequencyRate
                                                         , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 45.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(distance)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedM1M2DriveWithSignedSpeedAccelAndDistance( address: Byte
                                                           , distances: TwoMotorData[Int]
                                                           , speeds: TwoMotorData[Frequency]
                                                           , accel: FrequencyRate
                                                           , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 46.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(distances.m1)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(distances.m1)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class ReadBufferLength(address: Byte) extends Command[TwoMotorData[Option[Byte]]] {
  val command = 47.toByte
  def parseResults(data: ByteBuffer) = {
    val m1 = data.get
    val m2 = data.get
    Try(TwoMotorData( if (m1 == 0x80.toByte) None else Some(m1)
                    , if (m2 == 0x80.toByte) None else Some(m2)))
  }
}

case class DriveM1M2WithSignedSpeedAndIndividualAcceleration( address: Byte
                                                            , speeds: TwoMotorData[Frequency]
                                                            , accels: TwoMotorData[FrequencyRate])
           extends CrcCommand {
  val command = 50.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accels.m1.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(accels.m2.toHertzPerSecond.toInt)
    buf.putInt(speeds.m2.toHertz.toInt)
  }
}

case class BufferedDriveM1M2WithSignedSpeedIndividualAccelAndDistance( address: Byte
                                                                     , distances: TwoMotorData[Int]
                                                                     , speeds: TwoMotorData[Frequency]
                                                                     , accels: TwoMotorData[FrequencyRate]
                                                                     , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 51.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accels.m1.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(distances.m1)
    buf.putInt(accels.m2.toHertzPerSecond.toInt)
    buf.putInt(speeds.m2.toHertz.toInt)
    buf.putInt(distances.m2)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class DriveM1WithSignedDutyAndAcceleration( address: Byte
                                               , dutyCycle: Double
                                               , accel: FrequencyRate) extends CrcCommand {
  val command = 52.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycle * 32767).toShort)
    buf.putShort(accel.toHertzPerSecond.toShort)
  }
}

case class DriveM2WithSignedDutyAndAcceleration( address: Byte
                                               , dutyCycle: Double
                                               , accel: FrequencyRate) extends CrcCommand {
  val command = 53.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycle * 32767).toShort)
    buf.putShort(accel.toHertzPerSecond.toShort)
  }
}

case class DriveM1M2WithSignedDutyAndAcceleration( address: Byte
                                                 , dutyCycles: TwoMotorData[Double]
                                                 , accels: TwoMotorData[FrequencyRate])
           extends CrcCommand {
  val command = 54.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putShort((dutyCycles.m1 * 32767).toShort)
    buf.putShort(accels.m1.toHertzPerSecond.toShort)
    buf.putShort((dutyCycles.m1 * 32767).toShort)
    buf.putShort(accels.m1.toHertzPerSecond.toShort)
  }
}

case class PidConstants(p: Int, i: Int, d: Int) {}

case class ReadMotor1VelocityPidAndQppsSettings(address: Byte) extends Command[(PidConstants, Frequency)] {
  val command = 55.toByte
  def parseResults(data: ByteBuffer) = {
    Try(( PidConstants(data.getInt, data.getInt, data.getInt)
        , data.getInt.hertz))
  }
}

case class ReadMotor2VelocityPidAndQppsSettings(address: Byte) extends Command[(PidConstants, Frequency)] {
  val command = 56.toByte
  def parseResults(data: ByteBuffer) = {
    Try(( PidConstants(data.getInt, data.getInt, data.getInt)
        , data.getInt.hertz))
  }
}

case class SetMotor1PositionPidConstants( address: Byte
                                        , constants: PidConstants
                                        , maxI: Int = Int.MaxValue
                                        , deadzone: Int = 0
                                        , minPosition: Int = 0
                                        , maxPosition: Int = -1)  // really MaxValue
           extends CrcCommand {
  val command = 61.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(constants.d)
    buf.putInt(constants.p)
    buf.putInt(constants.i)
    buf.putInt(maxI)
    buf.putInt(deadzone)
    buf.putInt(minPosition)
    buf.putInt(maxPosition)
  }
}

case class SetMotor2PositionPidConstants( address: Byte
                                        , constants: PidConstants
                                        , maxI: Int = Int.MaxValue
                                        , deadzone: Int = 0
                                        , minPosition: Int = 0
                                        , maxPosition: Int = -1)  // really MaxValue
           extends CrcCommand {
  val command = 62.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(constants.d)
    buf.putInt(constants.p)
    buf.putInt(constants.i)
    buf.putInt(maxI)
    buf.putInt(deadzone)
    buf.putInt(minPosition)
    buf.putInt(maxPosition)
  }
}

case class PositionPidConstants( pid: PidConstants
                               , maxI: Int
                               , deadzone: Int
                               , positionRange: Range[Int]) {}

case class ReadMotor1PositionPidConstants(address: Byte) extends Command[PositionPidConstants] {
  val command = 63.toByte
  def parseResults(data: ByteBuffer) = {
    Try(PositionPidConstants( PidConstants(data.getInt, data.getInt, data.getInt)
                            , data.getInt
                            , data.getInt
                            , Range(data.getInt, data.getInt)))
  }
}

case class ReadMotor2PositionPidConstants(address: Byte) extends Command[PositionPidConstants] {
  val command = 64.toByte
  def parseResults(data: ByteBuffer) = {
    Try(PositionPidConstants( PidConstants(data.getInt, data.getInt, data.getInt)
                            , data.getInt
                            , data.getInt
                            , Range(data.getInt, data.getInt)))
  }
}

case class BufferedDriveM1WithSignedSpeedAccelDeccelAndPosition( address: Byte
                                                               , accel: FrequencyRate
                                                               , deccel: FrequencyRate
                                                               , speed: Frequency
                                                               , position: Int
                                                               , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 65.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(deccel.toHertzPerSecond.toInt)
    buf.putInt(position)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedDriveM2WithSignedSpeedAccelDeccelAndPosition( address: Byte
                                                               , accel: FrequencyRate
                                                               , deccel: FrequencyRate
                                                               , speed: Frequency
                                                               , position: Int
                                                               , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 66.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
    buf.putInt(speed.toHertz.toInt)
    buf.putInt(deccel.toHertzPerSecond.toInt)
    buf.putInt(position)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class BufferedDriveM1M2WithSignedSpeedAccelDeccelAndPosition( address: Byte
                                                                 , accels: TwoMotorData[FrequencyRate]
                                                                 , deccels: TwoMotorData[FrequencyRate]
                                                                 , speeds: TwoMotorData[Frequency]
                                                                 , positions: TwoMotorData[Int]
                                                                 , clearBuffer: Boolean = false)
           extends CrcCommand {
  val command = 67.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accels.m1.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(deccels.m1.toHertzPerSecond.toInt)
    buf.putInt(positions.m1)
    buf.putInt(accels.m1.toHertzPerSecond.toInt)
    buf.putInt(speeds.m1.toHertz.toInt)
    buf.putInt(deccels.m1.toHertzPerSecond.toInt)
    buf.putInt(positions.m1)
    buf.put(if(clearBuffer) 1.toByte else 0.toByte)
  }
}

case class SetM1DefaultDutyAcceleration( address: Byte
                                       , accel: FrequencyRate) extends CrcCommand {
  val command = 68.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
  }
}

case class SetM2DefaultDutyAcceleration( address: Byte
                                       , accel: FrequencyRate) extends CrcCommand {
  val command = 69.toByte
  override def populateBufferMiddle(buf: ByteBuffer): Unit = {
    buf.putInt(accel.toHertzPerSecond.toInt)
  }
}




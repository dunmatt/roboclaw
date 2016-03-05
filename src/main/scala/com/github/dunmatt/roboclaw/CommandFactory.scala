package com.github.dunmatt.roboclaw

import squants.electro.ElectricCurrent
import squants.time.Frequency

// NOTE: these are not all of the available commands, these are only the channel dependent commands

trait CommandFactory {
  def driveForward(speed: Byte): SimpleMotorCommand
  def driveBackwards(speed: Byte): SimpleMotorCommand
  def drive(speed: Byte): SimpleMotorCommand
  def setEncoderMode(mode: EncoderMode): CrcCommand
  def setCurrentLimit(max: ElectricCurrent): CrcCommand
  def readCurrentLimit: Command[ElectricCurrent]
  def readEncoder: Command[(Long, EncoderStatus)]
  def readSpeed: Command[(Frequency, EncoderStatus)]
  def setQuadratureEncoderValue(value: Long): CrcCommand
  def setVelocityPidConstants(qpps: Frequency, pid: PidConstants): CrcCommand = setVelocityPidConstants(qpps, pid.p, pid.i, pid.d)
  def setVelocityPidConstants(qpps: Frequency, p: Int, i: Int, d: Int): CrcCommand
  def readRawSpeed: Command[(Frequency, Boolean)]  // boolean here is "forward"
  def driveWithSignedDutyCycle(dutyCycle: Double): CrcCommand
  def driveWithSignedSpeed(speed: Frequency): CrcCommand
  def driveWithSignedSpeedAndAcceleration(speed: Frequency, accel: FrequencyRate): CrcCommand
  def bufferedDriveWithSignedSpeedAndDistance(speed: Frequency, dist: Int, clear: Boolean): CrcCommand
  def bufferedDriveWithSignedSpeedAccelAndDistance(speed: Frequency, dist: Int, accel: FrequencyRate, clear: Boolean): CrcCommand
  def driveWithSignedDutyAndAcceleration(dutyCycle: Double, accel: FrequencyRate): CrcCommand
  def readVelocityPidAndQppsSettings: Command[(PidConstants, Frequency)]
  // TODO: refactor this to use PositionPidConstants
  def setPositionPidConstants(gains: PidConstants, maxI: Int, deadzone: Int, minPos: Int, maxPos: Int): CrcCommand
  def readPositionPidConstants: Command[PositionPidConstants]
  def bufferedDriveWithSignedSpeedAccellDeccelAndPosition(accel: FrequencyRate, deccel: FrequencyRate, speed: Frequency, pos: Int, clear: Boolean): CrcCommand
  def setDefaultDutyAcceleration(accel: FrequencyRate): CrcCommand
}

object CommandFactory {
  def apply(addr: Byte, channel1: Boolean): CommandFactory = channel1 match {
    case true => new M1CommandFactory(addr)
    case false => new M2CommandFactory(addr)
  }
}

protected class M1CommandFactory(addr: Byte) extends CommandFactory {
  def driveForward(speed: Byte) = DriveForwardM1(addr, speed)
  def driveBackwards(speed: Byte) = DriveBackwardsM1(addr, speed)
  def drive(speed: Byte) = DriveM1(addr, speed)
  def setEncoderMode(mode: EncoderMode) = SetMotor1EncoderMode(addr, mode)
  def setCurrentLimit(max: ElectricCurrent) = SetM1CurrentLimit(addr, max)
  def readCurrentLimit = ReadM1CurrentLimit(addr)
  def readEncoder = ReadM1Encoder(addr)
  def readSpeed = ReadM1Speed(addr)
  def setQuadratureEncoderValue(value: Long) = SetQuadratureEncoder1Value(addr, value)
  def setVelocityPidConstants(qpps: Frequency, p: Int, i: Int, d: Int) = SetVelocityPidConstantsM1(addr, qpps, p, i, d)
  def readRawSpeed = ReadRawSpeedM1(addr)
  def driveWithSignedDutyCycle(dutyCycle: Double) = DriveM1WithSignedDutyCycle(addr, dutyCycle)
  def driveWithSignedSpeed(speed: Frequency) = DriveM1WithSignedSpeed(addr, speed)
  def driveWithSignedSpeedAndAcceleration(speed: Frequency, accel: FrequencyRate) = DriveM1WithSignedSpeedAndAcceleration(addr, speed, accel)
  def bufferedDriveWithSignedSpeedAndDistance(speed: Frequency, dist: Int, clear: Boolean) = BufferedM1DriveWithSignedSpeedAndDistance(addr, speed, dist, clear)
  def bufferedDriveWithSignedSpeedAccelAndDistance(speed: Frequency, dist: Int, accel: FrequencyRate, clear: Boolean) = BufferedM1DriveWithSignedSpeedAccelAndDistance(addr, dist, speed, accel, clear)
  def driveWithSignedDutyAndAcceleration(dutyCycle: Double, accel: FrequencyRate) = DriveM1WithSignedDutyAndAcceleration(addr, dutyCycle, accel)
  def readVelocityPidAndQppsSettings = ReadMotor1VelocityPidAndQppsSettings(addr)
  def setPositionPidConstants(gains: PidConstants, maxI: Int, deadzone: Int, minPos: Int, maxPos: Int) = SetMotor1PositionPidConstants(addr, gains, maxI, deadzone, minPos, maxPos)
  def readPositionPidConstants = ReadMotor1PositionPidConstants(addr)
  def bufferedDriveWithSignedSpeedAccellDeccelAndPosition(accel: FrequencyRate, deccel: FrequencyRate, speed: Frequency, pos: Int, clear: Boolean) = BufferedDriveM1WithSignedSpeedAccelDeccelAndPosition(addr, accel, deccel, speed, pos, clear)
  def setDefaultDutyAcceleration(accel: FrequencyRate) = SetM1DefaultDutyAcceleration(addr, accel)
}

protected class M2CommandFactory(addr: Byte) extends CommandFactory {
  def driveForward(speed: Byte) = DriveForwardM2(addr, speed)
  def driveBackwards(speed: Byte) = DriveBackwardsM2(addr, speed)
  def drive(speed: Byte) = DriveM2(addr, speed)
  def setEncoderMode(mode: EncoderMode) = SetMotor2EncoderMode(addr, mode)
  def setCurrentLimit(max: ElectricCurrent) = SetM2CurrentLimit(addr, max)
  def readCurrentLimit = ReadM2CurrentLimit(addr)  // NOTE: this command may not work, see TODO in Command.scala
  def readEncoder = ReadM2Encoder(addr)
  def readSpeed = ReadM2Speed(addr)
  def setQuadratureEncoderValue(value: Long) = SetQuadratureEncoder2Value(addr, value)
  def setVelocityPidConstants(qpps: Frequency, p: Int, i: Int, d: Int) = SetVelocityPidConstantsM2(addr, qpps, p, i, d)
  def readRawSpeed = ReadRawSpeedM2(addr)
  def driveWithSignedDutyCycle(dutyCycle: Double) = DriveM2WithSignedDutyCycle(addr, dutyCycle)
  def driveWithSignedSpeed(speed: Frequency) = DriveM2WithSignedSpeed(addr, speed)
  def driveWithSignedSpeedAndAcceleration(speed: Frequency, accel: FrequencyRate) = DriveM2WithSignedSpeedAndAcceleration(addr, speed, accel)
  def bufferedDriveWithSignedSpeedAndDistance(speed: Frequency, dist: Int, clear: Boolean) = BufferedM2DriveWithSignedSpeedAndDistance(addr, speed, dist, clear)
  def bufferedDriveWithSignedSpeedAccelAndDistance(speed: Frequency, dist: Int, accel: FrequencyRate, clear: Boolean) = BufferedM2DriveWithSignedSpeedAccelAndDistance(addr, dist, speed, accel, clear)
  def driveWithSignedDutyAndAcceleration(dutyCycle: Double, accel: FrequencyRate) = DriveM2WithSignedDutyAndAcceleration(addr, dutyCycle, accel)
  def readVelocityPidAndQppsSettings = ReadMotor2VelocityPidAndQppsSettings(addr)
  def setPositionPidConstants(gains: PidConstants, maxI: Int, deadzone: Int, minPos: Int, maxPos: Int) = SetMotor2PositionPidConstants(addr, gains, maxI, deadzone, minPos, maxPos)
  def readPositionPidConstants = ReadMotor2PositionPidConstants(addr)
  def bufferedDriveWithSignedSpeedAccellDeccelAndPosition(accel: FrequencyRate, deccel: FrequencyRate, speed: Frequency, pos: Int, clear: Boolean) = BufferedDriveM2WithSignedSpeedAccelDeccelAndPosition(addr, accel, deccel, speed, pos, clear)
  def setDefaultDutyAcceleration(accel: FrequencyRate) = SetM2DefaultDutyAcceleration(addr, accel)
}

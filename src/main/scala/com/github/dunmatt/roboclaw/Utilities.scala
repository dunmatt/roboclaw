package com.github.dunmatt.roboclaw

object Utilities {
  def crc16(packet: Seq[Byte]): Char = {
    packet.foldLeft(0.toChar) { case (crc, b) =>
      (0 until 8).foldLeft((crc ^ b).toChar) {
        case (c, _) if (c & 0x8000) > 0 => ((c << 1) ^ 0x1021).toChar
        case (c, _) => (c << 1).toChar
      }
    }
  }

  def withCrc(packet: Seq[Byte]): Seq[Byte] = {
    val crc = crc16(packet)
    packet :+ (crc >> 8).toByte :+ crc.toByte
  }

  def readSignedShort(bytes: Seq[Byte]): Short = ((bytes(0) << 8) | bytes(1)).toShort

  def writeSignedShort(value: Double): Seq[Byte] = {
    List((value.toShort >>> 8).toByte, value.toByte)
  }
}

package com.github.dunmatt.roboclaw

import java.nio.ByteBuffer

object Utilities {
  def crc16(packet: ByteBuffer): Char = {
    (0 until packet.position).foldLeft(0.toChar) { 
      case (crc, i) => crc16Step(crc, packet.get(i))
    }
  }

  def crc16Step(crc: Char, datum: Byte): Char = {
    (0 until 8).foldLeft((crc ^ ((datum & 0xFF) << 8)).toChar) {
      case (c, _) if (c & 0x8000) == 0 => (c << 1).toChar
      case (c, _) => ((c << 1) ^ 0x1021).toChar
    }
  }

  def verifyChecksum(address: Byte, command: Byte, packet: ByteBuffer): Boolean = {
    val target = packet.getChar(packet.position-2)
    val a = crc16Step(0, address)
    val b = crc16Step(a, command)
    (0 until packet.position-2).foldLeft(b) {
      case (crc, i) => crc16Step(crc, packet.get(i))
    } == target
  }
}

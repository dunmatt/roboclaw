package com.github.dunmatt.roboclaw

import java.nio.ByteBuffer

object Utilities {
  def crc16(packet: ByteBuffer): Char = {
    packet.flip
    (0 until packet.remaining).foldLeft(0.toChar) { case (crc, _) =>
      (0 until 8).foldLeft((crc ^ (packet.get & 0xFF)).toChar) {
        case (c, _) if (c & 0x8000) == 0 => (c << 1).toChar
        case (c, _) => ((c << 1) ^ 0x1021).toChar
      }
    }
  }
}

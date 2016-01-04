package com.github.dunmatt.roboclaw

import java.nio.ByteBuffer

object Utilities {
  def crc16(packet: ByteBuffer, dataLength: Int): Char = {
    (0 until dataLength).foldLeft(0.toChar) { case (crc, i) =>
      (0 until 8).foldLeft((crc ^ (packet.get(i) & 0xFF)).toChar) {
        case (c, _) if (c & 0x8000) == 0 => (c << 1).toChar
        case (c, _) => ((c << 1) ^ 0x1021).toChar
      }
    }
  }
}

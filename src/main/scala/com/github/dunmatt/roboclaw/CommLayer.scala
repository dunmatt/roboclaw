package com.github.dunmatt.roboclaw

trait CommLayer {
  def sendCommand[R](cmd: Command[R]): Future[R]
}

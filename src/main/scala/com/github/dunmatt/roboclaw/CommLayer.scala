package com.github.dunmatt.roboclaw

import scala.concurrent.Future

trait CommLayer {
  def sendCommand[R](cmd: Command[R]): Future[R]
}

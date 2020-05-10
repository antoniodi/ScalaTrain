package com.lightbend.training.scalatrain

class Time(val hours: Int = 0, val minutes: Int = 0) {
  //TODO: comment: Verify that hours is within 0 and 23
  require(hours >= 0 && hours < 24, "Invalid hours, must be >= 0 and < 24")

  //TODO: comment: Verify that minutes is within 0 and 59
  require(minutes >= 0 && hours < 60, "Invalid minutes, must be >= 0 and < 60")

  val asMinutes: Int = hours * 60 + minutes

  def minus(that: Time): Int = this.asMinutes - that.asMinutes

  def -(that: Time): Int = minus(that)
}

object Time {
  def fromMinutes(minutes: Int): Time = new Time(minutes / 60, minutes % 60)
}



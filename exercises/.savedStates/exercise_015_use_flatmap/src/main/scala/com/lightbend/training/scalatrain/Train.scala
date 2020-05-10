package com.lightbend.training.scalatrain

case class Train(kind: String, number: Int, schedule: Seq[(Time, Station)]) {
  require(schedule.size > 2, "schedule must contain at least 2 elements.")

  val stations: Seq[Station] = schedule.map(_._2)
}
package com.lightbend.training.scalatrain

case class Train(info: TrainInfo, schedule: Seq[(Time, Station)]) {
  require(schedule.size > 2, "schedule must contain at least 2 elements.")

  val stations: Seq[Station] = schedule.map(_._2)

  def timeAt(station: Station): Option[Time] = {
    schedule.find(_._2 == station).map(_._1)
    schedule.find{
      case (_, candidateStation) => candidateStation == station
    }.map{
      case (time, _) => time
    }
  }
}
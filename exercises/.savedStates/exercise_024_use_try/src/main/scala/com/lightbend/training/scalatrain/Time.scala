package com.lightbend.training.scalatrain

import play.api.libs.json.{JsError, JsNumber, JsObject, JsSuccess, JsValue, Json}

import scala.util.{Failure, Success, Try}

// Predef scala

case class Time(hours: Int = 0, minutes: Int = 0) extends Ordered[Time] {
  //TODO: comment: Verify that hours is within 0 and 23
  require(hours >= 0 && hours < 24, "Invalid hours, must be >= 0 and < 24")

  //TODO: comment: Verify that minutes is within 0 and 59
  require(minutes >= 0 && minutes < 60, "Invalid minutes, must be >= 0 and < 60")

  val asMinutes: Int = hours * 60 + minutes

  def minus(that: Time): Int = this.asMinutes - that.asMinutes

  def -(that: Time): Int = minus(that)

  override lazy val toString = f"$hours%02d:$minutes%02d"

  override def compare(that: Time): Int = this.asMinutes - that.asMinutes

  def toJson: JsValue = Json.obj("hours" -> hours, "minutes" -> minutes)

  def toJsonPropose: JsValue = JsObject(Map("hours" -> JsNumber(hours), "minutes" -> JsNumber(minutes)))
}

object Time {
  def fromMinutes(minutes: Int): Time = Time(minutes / 60, minutes % 60)

  def fromJson(time: JsValue): Option[Time] = {
    ((time \ "hours").validate[Int], (time \ "minutes").validate[Int]) match {
      case (JsError(_), _) => None
      case (JsSuccess(hours, _), JsError(_)) => Some(Time(hours))
      case (JsSuccess(hours, _), JsSuccess(minutes, _)) => Some(Time(hours, minutes))
    }
  }

  def fromJsonPropose(js: JsValue): Option[Time] = {
    for {
      hours <- Try((js  \ "hours").as[Int])
      minutes <- Try((js  \ "minutes").as[Int]).recover{ case _: Exception => 0 }
//      minutes <- Try((js  \ "minutes").as[Int]) match {
//        case Success(minutes) => Success(minutes)
//        case Failure(_) => Success(0)
//      }
    } yield Time(hours, minutes)
  }.toOption
}

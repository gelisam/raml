package com.keatext.gelineau.samuel.hello

import spray.json.DefaultJsonProtocol._
import spray.json._


sealed trait DataType
case object StringType extends DataType
case object NumberType extends DataType
case class DateType(
  dateFormat: String
) extends DataType {
  require(
    dateFormat match {
      case DateType.DateFormatPattern() => true
      case _ => false
    }
  )
}
case object BooleanType extends DataType

object DataType {
  implicit val jsonFormat: RootJsonFormat[DataType] = new RootJsonFormat[DataType] {
    override def read(json: JsValue): DataType =
      json match {
        case JsObject(jsObject) =>
          jsObject("discriminator") match {
            case JsString("StringType") =>
              StringType
            case JsString("NumberType") =>
              NumberType
            case JsString("DateType") =>
              val dateFormat = implicitly[JsonFormat[String]].read(jsObject("dateFormat"))
              DateType(dateFormat)
            case JsString("BooleanType") =>
              BooleanType
            case discriminator =>
              deserializationError(s"unexpected discriminator ${discriminator}, expected one of ${JsString("StringType")}, ${JsString("NumberType")}, ${JsString("DateType")} or ${JsString("BooleanType")}")
          }
        case _ =>
          deserializationError(s"expected object, got ${json}")
      }

    override def write(obj: DataType): JsValue =
      obj match {
        case StringType =>
          JsObject("discriminator" -> JsString("StringType"))
        case NumberType =>
          JsObject("discriminator" -> JsString("NumberType"))
        case DateType(dateFormat) =>
          JsObject(
            "discriminator" -> JsString("DateType"),
            "dateFormat" -> JsString(dateFormat)
          )
        case BooleanType =>
          JsObject("discriminator" -> JsString("BooleanType"))
      }
  }
}

object DateType {
  val DateFormatPattern = "[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r
}


case class Field(
  name: String,
  dataType: DataType
)

object Field {
  implicit val jsonFormat: RootJsonFormat[Field] = jsonFormat2(Field.apply)
}

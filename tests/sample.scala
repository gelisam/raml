package com.keatext.gelineau.samuel.hello


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

object DateType {
  val DateFormatPattern = "[YMD]+[-\\.][YMD]+[-\\.\\/][YMD]+".r
}


case class Field(
  name: String,
  dataType: DataType
)

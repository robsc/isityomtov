package com.rschonberger.isityomtov

import org.joda.time.format.DateTimeFormat
import org.joda.time.DateTime

object YomTovDateFormatter {
  val formatter = DateTimeFormat.forPattern("dd/MM/YYYY")

  def turnLineToDate(dateLine: String): DateTime = formatter.parseDateTime(dateLine)
}

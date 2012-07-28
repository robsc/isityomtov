package com.rschonberger.isityomtov

object YomTovStatus extends Enumeration {
  type YomTovStatus = Value
  val No, Yes = Value
  val FromSunset = Value("From sunset on")
  val UntilSundown = Value("Until sundown")
  val Unknown = Value("I Don't know!")
}

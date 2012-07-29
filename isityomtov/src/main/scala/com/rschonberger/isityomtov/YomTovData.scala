package com.rschonberger.isityomtov

import org.scala_tools.time.Imports._
import collection.immutable.{HashMap, HashSet}

object YomTovData {
  val dateOrdering: Ordering[DateTime] = Ordering.fromLessThan((_: DateTime) < (_: DateTime))
}

class YomTovData(private val config_file_data: Iterator[String]) {
  val infos: Map[DateTime, YomTovInfo] = createInfos
  val dates: Set[DateTime] = HashSet.empty ++ infos.keys
  val lastDate: DateTime = dates.max(YomTovData.dateOrdering)

  private def nextSaturday(date: DateTime): DateTime = {
    val dayOfWeek: Int = date.getDayOfWeek()
    // This just adds the right number of days.
    val daysToAdd: Int = (13 - dayOfWeek) % 7
    date + daysToAdd.day
  }

  private def saturdays(firstSaturday: DateTime, lastDay: DateTime): Stream[DateTime] = {
    if (firstSaturday <= lastDay)
      firstSaturday #:: saturdays(firstSaturday + 7.day, lastDay)
    else
      Stream.empty
  }

  private def shabbatInfos(firstSaturday: DateTime, lastDay: DateTime, takenDays: Set[DateTime]) = {
    for (saturday <- saturdays(firstSaturday, lastDay) if !takenDays.contains(saturday)) yield {
      val shabbos = new YomTovInfo(saturday, "It is the Sabbath", "http://www.youtube.com/watch?v=GPo9OBrIOi4")
      saturday -> shabbos
    }
  }

  private def createInfos: Map[DateTime, YomTovInfo] = {
    val info_list = config_file_data map TurnLineToInfo
    val info_mapped = HashMap.empty ++ (info_list map (x => (x.date -> x)))
    val dates = info_mapped keys

    val minSaturday: DateTime = nextSaturday(dates.min(YomTovData.dateOrdering))
    val maxDate: DateTime = dates.max(YomTovData.dateOrdering)
    HashMap.empty ++ info_mapped ++ shabbatInfos(minSaturday, maxDate, HashSet.empty ++ dates)
  }

  private def TurnLineToInfo(line: String): YomTovInfo = {
    val info = line split ','
    val date = YomTovDateFormatter.turnLineToDate(info(0))
    new YomTovInfo(date, info(1), info(2))
  }
}

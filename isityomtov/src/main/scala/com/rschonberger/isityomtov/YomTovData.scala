package com.rschonberger.isityomtov

import org.joda.time.DateTime
import collection.immutable.{HashMap, HashSet}

class YomTovData(private val config_file_data: Iterator[String]) {
  val infos: Map[DateTime, YomTovInfo] = createInfos
  val dates: Set[DateTime] = HashSet.empty ++ infos.keys

  private def createInfos: Map[DateTime, YomTovInfo] = {
    val info_list = config_file_data map TurnLineToInfo
    val info_mapped = info_list map (x => (x.date -> x))
    HashMap.empty ++ info_mapped
  }

  private def TurnLineToInfo(line: String): YomTovInfo = {
    val info: Array[String] = line split ','
    val date = YomTovDateFormatter.turnLineToDate(info(0))
    new YomTovInfo(date, info(1), info(2))
  }
}

package com.rschonberger.isityomtov

import javax.servlet.ServletConfig
import org.joda.time.DateTime

import scala.io.Source
import org.scalatra.ScalatraServlet
import org.scalatra.scalate.ScalateSupport
import collection.immutable.{HashSet, HashMap}
import org.joda.time.format.DateTimeFormat

class YomTovData(private val config_file_data: Iterator[String]) {
  val infos: Map[DateTime, YomTovInfo] = createInfos
  val dates: Set[DateTime] = HashSet.empty ++ infos.keys

  private def createInfos: Map[DateTime, YomTovInfo] = {
    val info_list = config_file_data map TurnLineToInfo
    val info_mapped = info_list map (x => (x.date -> x))
    HashMap.empty ++ info_mapped
  }

  private def TurnLineToDate(dateLine: String) = YomTovServlet.formatter parseDateTime dateLine

  private def TurnLineToInfo(line: String): YomTovInfo = {
    val info: Array[String] = line split ','
    val date = TurnLineToDate(info(0))
    new YomTovInfo(date, info(1), info(2))
  }
}

object YomTovServlet {
  val formatter = DateTimeFormat.forPattern("dd/MM/YYYY")
}

class YomTovServlet extends ScalatraServlet with ScalateSupport {
  // Set to true as needed when developing.
  override def isDevelopmentMode = false

  var yomTovData: YomTovData = _

  private def TurnLineToInfo(line: String): YomTovInfo = {
    val info: Array[String] = line split ','
    val date = TurnLineToDate(info(0))
    new YomTovInfo(date, info(1), info(2))
  }

  private def getInfoForDate(date: DateTime): YomTovInfo = {
    lazy val nothing = new YomTovInfo(date, "It's nothing important. Get to work!", "http://en.wikipedia.org/wiki/Manual_labour")
    lazy val shabbos = new YomTovInfo(date, "It is the Sabbath", "http://www.youtube.com/watch?v=GPo9OBrIOi4")
    yomTovData.infos get date match {
      case Some(x) => x
      case _ => if (date.dayOfWeek.getAsText == "Saturday") shabbos else nothing
    }
  }

  private def TurnLineToDate(dateLine: String): DateTime = {
    YomTovServlet.formatter parseDateTime dateLine
  }

  override def init(config: ServletConfig): Unit = {
    val file_name: String = config getInitParameter "config-file"
    val file_data: Source = Source fromFile file_name
    val file_lines: Iterator[String] = file_data.getLines
    yomTovData = new YomTovData(file_lines)
    file_data.close()
    super.init(config)
  }

  import YomTovStatus._

  def isItYomTov(date: DateTime): (YomTovStatus, YomTovInfo) = {
    import org.scala_tools.time.Imports._
    lazy val todayInfo = getInfoForDate(date)
    lazy val tomorrowInfo = getInfoForDate(date + 1.day)
    if (date.getYear != 2012) {
      (Unknown, todayInfo)
    } else {
      (isItYomTovToday(date - 1.day), isItYomTovToday(date), isItYomTovToday(date + 1.day)) match {
        case (_, true, false) => (UntilSundown, todayInfo)
        case (_, true, true) => (Yes, todayInfo)
        case (_, false, true) => (FromSunset, tomorrowInfo)
        case _ => (No, todayInfo)
      }
    }
  }

  def isItYomTovToday(date: DateTime): Boolean = {
    if (yomTovData.dates contains date) {
      true
    } else {
      (date.dayOfWeek) getAsText match {
        case "Saturday" => true
        case _ => false
      }
    }
  }

  get("/teapot") {
    halt(418, <title>I'm a teapot</title>)
  }

  notFound {
    <h1>Not Found.</h1>
  }

  get("/") {
    contentType = "text/html"
    response.setHeader("Cache-control", "max-age=86400,public")
    templateEngine.layout("redirect.ssp")
  }

  get("/dm/:day/:month/:year") {
    val parsed_date: DateTime = TurnLineToDate("%s/%s/%s".format(params("day"), params("month"), params("year")))
    val (status, info) = (isItYomTov(parsed_date))
    val data: Map[String, AnyRef] = HashMap(("yomtov" -> (status toString)), ("info" -> info))
    contentType = "text/html"
    if (status != Unknown) {
      // Keep results as cacheable as possible
      response.setHeader("Cache-control", "max-age=86400,public")
    }
    templateEngine.layout("isit.ssp", data)
  }
}

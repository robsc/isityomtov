package com.rschonberger.isityomtov

import javax.servlet.ServletConfig
import org.joda.time.DateTime

import scala.io.Source
import org.scalatra.ScalatraServlet
import org.scalatra.scalate.ScalateSupport
import collection.immutable.{HashSet, HashMap}

class YomTovServlet extends ScalatraServlet with ScalateSupport {
  // Set to true as needed when developing.
  override def isDevelopmentMode = false

  var yomTovData: YomTovData = _

  private def TurnLineToInfo(line: String): YomTovInfo = {
    val info: Array[String] = line split ','
    val date = YomTovDateFormatter.turnLineToDate(info(0))
    new YomTovInfo(date, info(1), info(2))
  }

  private def getInfoForDate(date: DateTime): YomTovInfo = {
    lazy val nothing = new YomTovInfo(date, "It's nothing important. Get to work!", "http://en.wikipedia.org/wiki/Manual_labour")
    yomTovData.infos get date match {
      case Some(x) => x
      case _ => nothing
    }
  }

  override def init(config: ServletConfig): Unit = {
    val file_lines: scala.Iterator[String] = getLines(config)
    yomTovData = new YomTovData(file_lines)

    super.init(config)
  }


  def getLines(config: ServletConfig): scala.Iterator[String] = {
    val file_name: String = config getInitParameter "config-file"
    val file_data: Source = Source fromFile file_name
    val file_lines: Iterator[String] = file_data.getLines
    file_lines
  }

  import YomTovStatus._

  def isItYomTov(date: DateTime): (YomTovStatus, Option[YomTovInfo]) = {
    import org.scala_tools.time.Imports._
    lazy val todayInfo = Some(getInfoForDate(date))
    lazy val tomorrowInfo = Some(getInfoForDate(date + 1.day))
    if (date > yomTovData.lastDate) {
      (Unknown, None)
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
    val parsed_date: DateTime = YomTovDateFormatter.turnLineToDate("%s/%s/%s".format(params("day"), params("month"), params("year")))
    val (status: YomTovStatus.YomTovStatus, info: Option[YomTovInfo]) = isItYomTov(parsed_date)
    val data: Map[String, AnyRef] = HashMap(("yomtov" -> (status toString)), ("info" -> info))
    contentType = "text/html"
    if (status != Unknown) response.setHeader("Cache-control", "max-age=86400,public")
    templateEngine.layout("isit.ssp", data)
  }
}

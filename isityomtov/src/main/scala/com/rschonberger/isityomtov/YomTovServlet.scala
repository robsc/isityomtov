package com.rschonberger.isityomtov

import javax.servlet.http.HttpServletRequest
import javax.servlet.ServletConfig
import org.joda.time.DateTime
import org.scalatra._
import scala.io.Source
import org.scala_tools.time.Imports._
import scalate.ScalateSupport

object YomTovStatus extends Enumeration {
  type YomTovStatus = Value
  val No, Yes = Value
  val FromSunset = Value("From sunset on")
  val UntilSundown = Value("Until sundown")
  val Unknown = Value("I Don't know!")
}

class YomTovInfo (val date: DateTime, val description: String, val link: String)

class YomTovServlet extends ScalatraServlet with ScalateSupport {
  // Set to true as needed when developing.
  override def isDevelopmentMode = false
  private val formatter: org.joda.time.format.DateTimeFormatter = { 
    // Strings like "Monday, Feb 20, 2012"
    org.joda.time.format.DateTimeFormat.forPattern("dd/MM/YYYY")
  }
  var dates: Set[DateTime] = new scala.collection.immutable.HashSet[DateTime];
  var infos: Map[DateTime, YomTovInfo] = scala.collection.immutable.HashMap.empty[DateTime, YomTovInfo];

  private def TurnLineToInfo(line: String): YomTovInfo = { 
    val info: Array[String] = line split ','
    val date = TurnLineToDate (info(0))
    new YomTovInfo(date, info(1), info(2))
  }

  private def getInfoForDate(date: DateTime): YomTovInfo = { 
    lazy val nothing = new YomTovInfo(date, "It's nothing important. Get to work!", "http://en.wikipedia.org/wiki/Manual_labour")
    lazy val shabbos = new YomTovInfo(date, "It is the Sabbath", "http://www.youtube.com/watch?v=GPo9OBrIOi4")
    infos get date match { 
    case Some(x) => x
    case _ => if (date.dayOfWeek.getAsText  == "Saturday") shabbos else nothing
    }
  }

  private def TurnLineToDate(dateLine: String): DateTime = { 
    formatter parseDateTime dateLine
  }
  override def init(config: ServletConfig): Unit = { 
    val file_name: String = config getInitParameter "config-file"
    val file_data: Source = Source fromFile file_name;
    val info_list = ((file_data getLines) map TurnLineToInfo)
    infos ++= info_list map (x => (x.date -> x))
    dates ++= ((infos mapValues {x => x.date }) values)
    super.init(config)
  }

  import YomTovStatus._

  def isItYomTov(date: DateTime): (YomTovStatus, YomTovInfo) = { 
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

  def isItYomTovToday(date:DateTime):Boolean = {
    if (dates contains date) { 
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
    templateEngine.layout("redirect.ssp")
  }

  get("/dm/:day/:month/:year") { 
    val parsed_date : DateTime = TurnLineToDate("%s/%s/%s".format(params("day"), params("month"), params("year")))
    var data: Map[String, AnyRef] = Map.empty
    val (status, info) = (isItYomTov(parsed_date))
    data ++= Array(("yomtov" -> (status toString)), ("info" -> info))
    contentType = "text.html"
    templateEngine.layout("isit.ssp", data)
  }
}

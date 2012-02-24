package com.rschonberger.isityomtov

import javax.servlet.http.HttpServletRequest
import javax.servlet.ServletConfig
import org.joda.time.DateTime
import org.scalatra._
import scala.io.Source
import org.scala_tools.time.Imports._
import scalate.ScalateSupport

class YomTovServlet extends ScalatraServlet with ScalateSupport {
  
  private val formatter: org.joda.time.format.DateTimeFormatter = { 
    // Strings like "Monday, Feb 20, 2012"
    org.joda.time.format.DateTimeFormat.forPattern("dd/MM/YYYY")
  }
  var dates: Set[DateTime] = new scala.collection.immutable.HashSet[DateTime];

  private def TurnLineToDate(dateLine: String): DateTime = { 
    formatter parseDateTime dateLine
  }
  override def init(config: ServletConfig): Unit = { 
    val file_name: String = config getInitParameter "config-file"
    val file_data: Source = Source fromFile file_name;
    val mutable_dates: scala.collection.immutable.Set[DateTime] = new scala.collection.immutable.HashSet[DateTime]

    dates = mutable_dates ++ ((file_data getLines) map TurnLineToDate)
    for (d <- dates) { 
      println(d)
    }
  }

  object YomTovStatus extends Enumeration {
    type YomTovStatus = Value
    val No, Yes = Value
    val FromSunset = Value("From sunset on")
    val UntilSundown = Value("Until sundown")
    val Unknown = Value("I Don't know!")
  }
  import YomTovStatus._

  def isItYomTov(date: DateTime): YomTovStatus = { 
    if (date.getYear != 2012) { 
      Unknown
    } else { 
      (isItYomTovToday(date - 1.day), isItYomTovToday(date), isItYomTovToday(date + 1.day)) match { 
	case (_, true, false) => UntilSundown
	case (_, true, true) => Yes
	case (_, false, true) => FromSunset
	case _ => No
      }
    }
  }

  def isItYomTovToday(date:DateTime):Boolean = {
    println (date)
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
    // Simple javascript: open a GET url with the correct DATE.
    <html>
    <head>
    <script type="text/javascript">
    date = new Date();
    loc = window.location + 'dm/' + date.getDate() + '/' + (date.getMonth() + 1) + '/' + (date.getFullYear())
    window.location = loc
    </script>
    </head>
    </html>
  }

  get("/dm/:day/:month/:year") { 
    val parsed_date : DateTime = TurnLineToDate("%s/%s/%s".format(params("day"), params("month"), params("year")))
    <html>
      <body>
        <font size='+100'>
          <center>{isItYomTov(parsed_date)}</center>
        </font>
      </body>
    </html>
  }
}

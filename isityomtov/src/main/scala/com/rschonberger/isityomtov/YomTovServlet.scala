package com.rschonberger.isityomtov

import javax.servlet.http.HttpServletRequest
import org.scalatra._
import org.scala_tools.time.Imports._
import scalate.ScalateSupport

class YomTovServlet extends ScalatraServlet with ScalateSupport {
  object YomTovStatus extends Enumeration {
    type YomTovStatus = Value
    val No, Yes = Value
    val FromSunset = Value("From sunset on")
    val UntilSundown = Value("Until sundown")
  }
  import YomTovStatus._

  def isItYomTov(date:org.joda.time.DateTime):YomTovStatus = {
    (date.dayOfWeek) getAsText match { 
      case "Friday" => YomTovStatus.FromSunset
      case "Saturday" => YomTovStatus.UntilSundown
      case _ => No
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
    window.location = window.location + 'date/' + date.toLocaleDateString();
    </script>
    </head>
    </html>
  }

  private val formatter: org.joda.time.format.DateTimeFormatter = { 
    // Strings like "Monday, Feb 20, 2012"
    org.joda.time.format.DateTimeFormat.forPattern("EEE, MMM dd, YYYY")
  }
  
  get("/date/:date") {
    val date: String = params("date")
        val parsed_date: org.joda.time.DateTime = formatter  parseDateTime date
    <html>
      <body>
        <font size='+100'>
          <center>{isItYomTov(parsed_date)}</center>
        </font>
      </body>
    </html>
  }
}

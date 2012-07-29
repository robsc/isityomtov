package com.rschonberger.isityomtov

import org.scalatra.test.specs2._
import javax.servlet.ServletConfig

class NoConfigYomTovServlet extends YomTovServlet {
  override def getLines(config: ServletConfig): Iterator[String] = {
    val lines: String = """08/03/2012,Purim,http://en.wikipedia.org/wiki/Purim
                          |07/04/2012,Passover,http://en.wikipedia.org/wiki/Passover
                          |08/04/2012,Passover,http://en.wikipedia.org/wiki/Passover
                          |13/04/2012,Passover,http://en.wikipedia.org/wiki/Passover
                          |14/04/2012,Passover,http://en.wikipedia.org/wiki/Passover""".stripMargin
    lines.split("\n").iterator
  }
}

// For more on Specs2, see http://etorreborre.github.com/specs2/guide/org.specs2.guide.QuickStart.html 
class YomTovServletSpec extends ScalatraSpec { def is =
  "GET / on YomTovServlet"                     ^
    "should return status 200"                  ! root200^
                                                end
    
  addServlet(classOf[NoConfigYomTovServlet], "/*")

  def root200 = get("/") { 
    status must_== 200
  }
}

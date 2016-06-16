package pub.jayden.scala.sfjd.chapter3

import javax.servlet.http.{HttpServletResponse, HttpServletRequest, HttpServlet}

/**
 * Created by jaydenuk on 2016. 3. 2..
 */
class SimpleServlet extends HttpServlet{
  override def doGet(request: HttpServletRequest, response: HttpServletResponse) {
    response.setContentType("text/html")
    response.setCharacterEncoding("UTF-8")
    response.getWriter.write("""<html><body><h1>Hello, world!</h1></body></html>""")
  }
}

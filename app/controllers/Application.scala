package controllers

import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Akka
import play.api.libs.ws.{ WS, Response }
import akka.dispatch.Await
import play.api.libs.iteratee.Enumerator

object Application extends Controller {

  def index = Action { request =>
    Ok("Got request [" + request + "]")
  }

  def getTitleFromId(id: Long): Promise[String] = {
    val url = "http://en.wikipedia.org/w/api.php?format=xml&action=query&pageids=" + id
    WS.url(url).get().map { response =>
      (response.xml \\ "page" \ "@title").toString
    }
  }

  def getWikiHtmlPage(title: Promise[String]) = {
    val url = "http://en.wikipedia.org/wiki/Kouign_amann"
    WS.url(url).get()
  }

  def article(id: Long) = Action {
    val promiseOfTitle = getTitleFromId(id)
    val promiseOfHtml = getWikiHtmlPage(promiseOfTitle)
    Async {
      promiseOfHtml.map(n => SimpleResult(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/html")),
        body = Enumerator(n.body)))
    }
  }

}

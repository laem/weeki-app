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

  def articleFromId(id: Long) = {
    val url = "http://en.wikipedia.org/w/api.php?format=xml&action=query&pageids=" + id
      val promiseOfTitle = WS.url(url).get().map { response =>
        (response.xml \\ "page" \ "@title").toString
      }
    articleFromTitle(promiseOfTitle)
  }

  def articleFromTitle(title: Any) = Action {
    //TODO : correctly handle title formating
    val res = title match {
      case title: String =>  WS.url("http://en.m.wikipedia.org/wiki/" + title).get()
      case promise: Promise[String] => promise.flatMap(title => WS.url("http://en.m.wikipedia.org/wiki/" + title).get())
      // How to remove this warning ?
    }
    Async {
     
      res.map { n => SimpleResult(
        header = ResponseHeader(200, Map(CONTENT_TYPE -> "text/html")),
        body = Enumerator(n.body))
        }
    }

  }

}

package controllers

import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Promise
import play.api.libs.concurrent.Akka
import play.api.libs.ws.{ WS, Response }
import akka.dispatch.Await
import play.api.libs.iteratee.Enumerator
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.JsValue
import com.codahale.jerkson.Json._
import play.api.libs.json.Json
import scala.xml.NodeSeq
import scala.xml.Elem
import play.api.templates.Html
import scala.xml.XML
import play.api.Play.current
import akka.util.duration
import org.laem.weeki.{SearchAPIClient, Tweet}
import org.laem.weeki.searchTheFlock

object Application extends Controller {

  def index = Action { implicit request =>
    Ok(views.html.index("Welcome :)"))
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
      case title: String => WS.url("http://en.m.wikipedia.org/wiki/" + title).get()
      case promise: Promise[_] => promise.flatMap(title => WS.url("http://en.m.wikipedia.org/wiki/" + title).get())
    }
    Async { //Problem : Wikipedia's HTML is not valid XML, parser crashes...
      pageSplit(res).map { tuple =>
        Ok(views.html.main("Title to retrieve")(Html(tuple._1.toString))(Html(tuple._2.toString)))
      }
    }
  }

  def pageSplit(p: Promise[Response]): Promise[(NodeSeq, NodeSeq)] = { // Returns the head and body of an HTML page
    p.map { response =>
      (\*(scala.xml.XML.loadString(response.body.dropWhile(c => c != '\n').drop(1)) \ "html" \ "head"), \*(response.xml \ "html" \ "body"))
    }
  }

  def \*(ns: NodeSeq): NodeSeq = ns flatMap {
    _ match {
      case e: Elem => e.child
      case _ => NodeSeq.Empty
    }
  }

  def inlayTweets(n: Response) = {
    val titleNode = (n.xml \\ "h1" \ "@id") find { _.text == "firstHeading" } get
    val title = titleNode.text

    val tweets = <span>Tweets about @title will be displayed here</span>

    val content = (n.xml \\ "div" \ "@id") find { _.text == "content" }
    content.map(div => (div \\ "p").head)
  }

  def ws = WebSocket.using[JsValue] { request =>

    // Just consume and ignore the input, we only need to establish a connection
    val in = Iteratee.foreach[JsValue] { event =>

    }

    // Tweets will be sent through this channel
    // id_str: String, created_at: String, usr_name: String, text: String, concepts: List[String] 
    // All the information to display have to be there, we don't want to call the Twitter API client side. 
    val t = Tweet("81979798765", "Wed Aug 27 13:08:45 +0000 2008", "laem",
      "La voie pour l'avenir de l'humanité")

    //val out = Enumerator(Json.parse(generate(t))) >>> Enumerator.eof
    val out = Enumerator.fromCallback { () =>
      var p: Promise[JsValue] = searchTheFlock.go(List("Crepe")).map{ l => Json.toJson(l.map ( t => Json.parse(generate(t))))
        
      }
      p.flatMap(jsv => Promise.timeout(Some(jsv), akka.util.Duration(10, "seconds")))
    }
    (in, out)
  }

}

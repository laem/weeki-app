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
import scala.xml.{ NodeSeq, Elem, XML }
import play.api.templates.Html
import play.api.Play.current
import akka.util.duration
import org.laem.weeki.{ SearchAPIClient, Tweet }
import org.laem.weeki.searchTheFlock
import scala.xml.factory.XMLLoader
import org.ccil.cowan.tagsoup.jaxp.SAXFactoryImpl
import java.net.URLEncoder

// An annotated tweet, concepts are wikipedia article titles
case class AnTweet(t: Tweet, concepts: List[String])

// To parse the relevant information from WikiMiner's json answers
case class ServiceResponse(detectedTopics: List[Map[String, Any]], wikifiedDocument: String)

object TagSoupXmlLoader {

  private val factory = new SAXFactoryImpl()

  def get(): XMLLoader[Elem] = {
    XML.withSAXParser(factory.newSAXParser())
  }
}

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
    Async {
      pageSplit(res).map { tuple =>
        Ok(views.html.main("Weeki")(Html(tuple._1.toString))(Html(tuple._2.toString)))
      }
    }
  }

  def pageSplit(p: Promise[Response]): Promise[(NodeSeq, NodeSeq)] = {
    // Returns the head and body of an HTML page
    // TagSoup is used to get correctly formatted XML from HTML
    val loader = TagSoupXmlLoader.get

    p.map { response =>
      val page = loader.loadString(response.body)
      (\*(page \ "head"), \*(page \ "body"))
    }
  }

  def \*(ns: NodeSeq): NodeSeq = ns flatMap {
    _ match {
      case e: Elem => e.child
      case _ => NodeSeq.Empty
    }
  }

  def annotateTweets(l: List[Tweet]) = {
    val res = l.map { t =>
      val url = "http://wikipedia-miner.cms.waikato.ac.nz/services/wikify?source=" + URLEncoder.encode(t.text, "UTF-8") + "&responseFormat=xml&minProbability=0.4"
      val conceptsPromise = WS.url(url).get().map { response =>
        (response.xml \\ "detectedTopics" \ "detectedTopic").map(el => (el \ "@title").text)
      }
      conceptsPromise.map { concepts => AnTweet(t, concepts.toList) }
    }
    Promise.sequence(res)
  }

  def inlayTweets(n: Response) = {
    val titleNode = (n.xml \\ "h1" \ "@id") find { _.text == "firstHeading" } get
    val title = titleNode.text

    val tweets = <span>Tweets about @title will be displayed here</span>

    val content = (n.xml \\ "div" \ "@id") find { _.text == "content" }
    content.map(div => (div \\ "p").head)
  }

  def ws = WebSocket.using[JsValue] { request =>

    // Just get the title of the article
    var title: Option[String] = None

    val out = Enumerator.imperative[JsValue]()
    
    val in = Iteratee.foreach[JsValue] { message =>
      println("Annotating wiki article: " + message.toString)
      title = (message \ "title").asOpt[String]
      retrieveTweets(title).map(joptn => out.push(joptn.get))
    }

    // Tweets will be sent through this channel
    // All the information to display have to be there, we don't want to call the Twitter API client side. 

    /* example enumerator output
     * 
     * val t = Tweet("81979798765", "Wed Aug 27 13:08:45 +0000 2008", "laem",
     *  "La voie pour l'avenir de l'humanité")
	 *
     * val out = Enumerator(Json.parse(generate(t))) >>> Enumerator.eof
     */

    (in, out)
  }

  def retrieveTweets(title: Option[String]) = {
    if (title.isDefined) {
      searchTheFlock.go(List(title.get)).flatMap { tweetList =>
        println("Annotation start")
        annotateTweets(tweetList).map { l =>
          Json.toJson(l.map { t => println(t); Json.parse(generate(t)) })
        }
      }.flatMap(jsv => Promise.timeout(Option(jsv), akka.util.Duration(10, "seconds")))
    } else Promise.pure(None)
  }

}

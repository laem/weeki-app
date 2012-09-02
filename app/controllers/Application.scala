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
import com.codahale.jerkson.ParsingException
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
import com.twitter.util.Future
import play.api.data._
import play.api.data.Forms._

// A preview resulting from a Diffbot API call. Some fields are ignored, for their use is not obvious for now. 
abstract class Prev
case class Preview(resolved_url: String) extends Prev
case class PreviewError(error: String) extends Prev

// An annotated tweet, concepts are wikipedia article titles
case class AnTweet(t: Tweet, concepts: List[String], previews: List[Prev])

// To parse the relevant information from WikiMiner's json answers
case class ServiceResponse(detectedTopics: List[Map[String, Any]], wikifiedDocument: String)

case class EditText(text: String)

object TagSoupXmlLoader {

  private val factory = new SAXFactoryImpl()

  def get(): XMLLoader[Elem] = {
    XML.withSAXParser(factory.newSAXParser())
  }
}

object Application extends Controller {

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

  //Call the Wikipedia Miner annotation service for annotation of a text input
  def annotateText(input: String, minProb: Double) = {
    val aUrl = "http://wikipedia-miner.cms.waikato.ac.nz/services/wikify?source=" + URLEncoder.encode(input, "UTF-8") + "&responseFormat=xml&minProbability=" + minProb
    WS.url(aUrl).get().map { response =>
      (response.xml \\ "detectedTopics" \ "detectedTopic").map(el => (el \ "@title").text)
    }
  }

  //Call the Diffbot service to get a link preview
  def retrievePreview(url: String): Promise[Prev] = {
    val pUrl = "http://www.diffbot.com/api/article?token=1b88ca5d5b3bb3d385f3ff2bdb160bc7&url=" + url
    WS.url(pUrl).get().map { response =>
      try {
        val preview = com.codahale.jerkson.Json.parse[Preview](response.json.toString)
        preview
      } catch {
        case e: ParsingException =>
          println("JSON parse error while retrieving the preview" + e.getCause)
          val p = PreviewError("The preview for " + url + " could not be obtained")
          println("created a PreviewError instead")
          p
      }
    }
  }

  //Output a list of annotated and link previewed tweets
  def annotateTweets(l: List[Tweet], minProb: Double) = {
    val res = l.map { t =>
      val conceptsPromise = annotateText(t.text, minProb)
      val previews: Seq[Promise[Prev]] = t.entities.urls.map { url =>
        retrievePreview(url.url)
      }
      val prevsPromise = Promise.sequence(previews)

      conceptsPromise.flatMap { concepts =>
        prevsPromise.map { prevs =>
          AnTweet(t, concepts.toList, prevs.toList)
        }
      }
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

  //Websocket for a communication channel between the back-end and the client through javascript.
  def ws = WebSocket.using[JsValue] { request =>

    // Just get the title of the article
    var title: Option[String] = None

    val out = Enumerator.imperative[JsValue]()

    val in = Iteratee.foreach[JsValue] { message =>
      println("Annotating wiki article: " + message.toString)
      title = (message \ "title").asOpt[String]
      	Akka.system.scheduler.schedule( akka.util.Duration(0, "seconds"), akka.util.Duration(20, "seconds")) {
    	  retrieveTweets(title).map(joptn => out.push(joptn.get))  
		}	
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
      searchTheFlock.go(List(title.get), 10).flatMap { tweetList =>
        println("Annotation start")
        annotateTweets(tweetList, 0.3).map { l =>
          Json.toJson(l.map { t => println(t); Json.parse(generate(t)) })
        }
      }.map(jsv => Option(jsv))
    } else Promise.pure(None)
  }

  
  def annotatews = WebSocket.using[JsValue] { request =>

    // Just get the title of the article

    val out = Enumerator.imperative[JsValue]()

    val in = Iteratee.foreach[JsValue] { message =>
      //println("Annotating input text: " + message.toString)
      val input = (message \ "text").asOpt[String]
      if (input.isDefined) {
        annotateText(input.get, 0.4).map{ conceptSeq =>
          out.push(Json.toJson(conceptSeq.toList.map(concept => Json.parse(generate(concept)))))
        }
      }
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

 

  def index = Action { implicit request =>
    Ok(views.html.index("Yesssss !"))
  }

}

package org.laem.weeki

import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http
import com.twitter.conversions.time._
import java.net.InetSocketAddress
import java.net.URLEncoder
import org.jboss.netty.util.CharsetUtil
import org.jboss.netty.handler.codec.http._
import org.jboss.netty.handler.codec.http.HttpResponseStatus._
import com.twitter.finagle.{ Service, SimpleFilter }
import com.twitter.util.Future
import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import org.codehaus.jackson.map.JsonMappingException

/**
 * A somewhat advanced example of using Filters with Clients. Below, HTTP 4xx and 5xx
 * class requests are converted to Exceptions. Additionally, two parallel requests are
 * made and when they both return (the two Futures are joined) the TCP connection(s)
 * are closed.
 */

//case class Tweet(id_str: String, created_at: String, from_user: String, text: String)
//case class JsonResponse(results: Seq[Tweet])

object SearchAPIClient {
  class InvalidRequest extends Exception

  /**
   * Convert HTTP 4xx and 5xx class responses into Exceptions.
   */
  class HandleErrors extends SimpleFilter[HttpRequest, HttpResponse] {
    def apply(request: HttpRequest, service: Service[HttpRequest, HttpResponse]) = {
      // flatMap asynchronously responds to requests and can "map" them to both
      // success and failure values:
      service(request) flatMap { response =>
        response.getStatus match {
          case OK => Future.value(response)
          case _ => Future.exception(new Exception(response.getStatus.getReasonPhrase))
        }
      }
    }
  }

  //Enclose multi word strings in ""
  def enclose(in: String): String = if (in.contains(' ')) "\"" + in + "\"" else in

  def go(keywords: List[String]) = {

    val clientWithoutErrorHandling: Service[HttpRequest, HttpResponse] = ClientBuilder()
      .codec(Http())
      .hosts(new InetSocketAddress("search.twitter.com", 80))
      .hostConnectionLimit(0)
      .tcpConnectTimeout(1.second)
      .build()

    val handleErrors = new HandleErrors

    // compose the Filter with the client:
    val client: Service[HttpRequest, HttpResponse] = handleErrors andThen clientWithoutErrorHandling

    //Contruct the Twitter Search API GET path from keywords

    val path = "/search.json?q=" + URLEncoder.encode(keywords.tail.foldLeft(enclose(keywords.head))((k, i) => k + " OR " + enclose(i)), "UTF-8")


    val request = makeRequest(client, path)

    // When both request1 and request2 have completed, close the TCP connection(s).
    (request) onSuccess { response =>
      val responseString = response.getContent.toString(CharsetUtil.UTF_8)
      println("))) Received result: " + responseString)
      //Parse response for Json objects
      var twts: List[Tweet] = Nil

      try {
        val tweets = parse[JsonResponse](responseString).results
        println(tweets)
        twts = tweets.take(5).toList
        println("twts "+twts)
        //TODO: flip pages

      } catch {
        case e: ParsingException => println("Parse error")
      }

    } 
  }

  private[this] def makeRequest(client: Service[HttpRequest, HttpResponse], path: String) = {
    val request = new DefaultHttpRequest(
      HttpVersion.HTTP_1_1, HttpMethod.GET, path)

    //println(request)
    client(request) onSuccess { response =>

    } onFailure { error =>
      println("))) Request Error: " + error.getClass.getName)
    }
  }

}

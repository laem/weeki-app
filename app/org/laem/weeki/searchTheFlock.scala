package org.laem.weeki
import java.net.URLEncoder
import play.api.libs.ws.WS
import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import play.api.libs.json.Json

object searchTheFlock {

  //Enclose multi word strings in ""
  def enclose(in: String): String = if (in.contains(' ')) "\"" + in + "\"" else in

  def go(keywords: List[String]) = {

    val path = "http://twitter.com/search.json?q=" + URLEncoder.encode(keywords.tail.foldLeft(enclose(keywords.head))((k, i) => k + " OR " + enclose(i)), "UTF-8")

    val response = WS.url(path).get()
    
    response.map { response =>
      try {
        val tweets = parse[JsonResponse](response.json.toString).results
        var twts: List[Tweet]= tweets.take(4).toList
        //TODO: flip pages
        twts
        
      } catch {
        case e: ParsingException => println("Parse error")
        Nil
      }
    }
  }

}
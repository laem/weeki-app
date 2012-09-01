package org.laem.weeki
import java.net.URLEncoder
import play.api.libs.ws.WS
import com.codahale.jerkson.Json._
import com.codahale.jerkson.ParsingException
import play.api.libs.json.Json

case class Url(url: String)
case class Entities(urls: Seq[Url])
case class Tweet(id_str: String, created_at: String, from_user: String, text: String, entities: Entities)
case class JsonResponse(results: Seq[Tweet])

//Twitter search API client
object searchTheFlock {

  //Enclose multi word strings in ""
  def enclose(in: String): String = if (in.contains(' ')) "\"" + in + "\"" else in

  def go(keywords: List[String], nb: Int) = {

    val path = "http://twitter.com/search.json?q=" + URLEncoder.encode(keywords.tail.foldLeft(enclose(keywords.head))((k, i) => k + " OR " + enclose(i)), "UTF-8") + "%20-RT" + "&rpp=" + nb + "&include_entities=1"
    		
    val response = WS.url(path).get()
    
    response.map { response =>
      try {
        val tweets = parse[JsonResponse](response.json.toString).results
        var twts: List[Tweet]= tweets.take(nb).toList
        //TODO: flip pages
        twts
        
      } catch {
        case e: ParsingException => println("Parse error in searchTheFlock"+e.getCause)
        Nil
      }
    }
  }

}
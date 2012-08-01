package org.laem.weeki


case class Tweet(id_str: String, created_at: String, from_user: String, text: String)
case class JsonResponse(results: Seq[Tweet])

object BirdiesCall{


  def go(title: String) = {
    // No anchor retrieving for now.
	//val res = AnchorSearch.search(args(0), args(1))
	//We directly search for the title.
    SearchAPIClient.go(List(title))
  }
}

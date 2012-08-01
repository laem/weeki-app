package org.laem.weeki

object BirdCall {

  def main(args: Array[String]): Unit = {

    /*
   * Old code, when we planned to get streaming tweets and to annotate them	
   * 
   * StreamReader.go
   * WikiMinerClient.go("Workin on cleaning out some past junk, then hope to be startin fresh and clean! :)")  
   * 
   * 
   */
	//val res = AnchorSearch.search(args(0), args(1))
	//println(res)
    //SearchAPIClient.go(res.map(_.productElement(0).toString))
    println(SearchAPIClient.go(List("Crepe")))
  }
}

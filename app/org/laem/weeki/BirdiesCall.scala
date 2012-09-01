package org.laem.weeki



object BirdiesCall{


  def go(title: String) = {
    // No anchor retrieving for now.
	//val res = AnchorSearch.search(args(0), args(1))
	//We directly search for the title.
    SearchAPIClient.go(List(title))
  }
}

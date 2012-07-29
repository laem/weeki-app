package org.laem.weeki

import scala.io.Source

object AnchorSearch {

  /*
   *  Look for a file named with the id number of the wikiepdia article
   *  Parse file
   *  Return List of anchors with their occurence count
   */

  def search(anchorDirectory: String, tid: String) = {
    
    Source.fromFile(anchorDirectory+tid).getLines.toList.map(
        _.split("\\t").toList match {
         	case a :: c :: Nil => a -> c.toInt
         	case _ => Nil
        }
      )
    
  }

}

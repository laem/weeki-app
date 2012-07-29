import scala.io._
import java.io._
import scala.collection.mutable.HashMap

/*
 * Reads the anchor file output of Wikipedia Preprocessor (http://www.cs.technion.ac.il/~gabr/resources/code/wikiprep/)
 * and dispatch the anchor texts and counts corresponding to a target ID in separate ID files.
 * The file size in reduced (no duplicates) and it is faster, easier to query by target ID.
 */
object Dispatcher {
  def main(args: Array[String]) {

    //ids as ints or strings ?
    case class Link(tid: String, oid: String, a: String)
    case class acount(a: String, c: Int)

    val iterator = Source.fromFile("anchors").getLines
    iterator.foreach { line =>
      var link = line.split("\\t").toList match {
        case tid :: oid :: a :: Nil => println("input read: "+Link(tid, oid, a));Link(tid, oid, a)
        case a => println("PB"); Link("there", "is", "a problem")
      }
      
      //The index file for article tid
      val tfile = new File(link.tid)
      //HashMap will hold the file contents  
      val map = new HashMap[String, Int]

      val file = new File(link.tid)
      file.createNewFile
      
        println("Reading target file")
        val it = Source.fromFile(link.tid).getLines

        //put the file contents in the hashmap
        it.foreach { line =>
          println("tid input:"+line.split("\\t").toList)
          val acount = line.split("\\t").toList match {
            case a :: c :: Nil => map.put(a, c.toInt)
            case what => println("This article file lines is strange: " + what)
          }
        }

        //remove spaces before and after the anchor before
        var a = link.a.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse

        val result = map.get(a) match {
          case Some(c: Int) => map += ((a, c + 1))
          case None => map += ((a, 1))
        }

        //write the new file
        file.delete
        val out = new PrintWriter(new FileWriter(file, true))
        map.foreach( couple =>
            out.println(couple._1 + "\t" + couple._2))
        out.close
    }
  }

}


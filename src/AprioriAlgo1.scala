import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.{Failure, Success, Try}
import util.control.Breaks._
import java.io._

object Apriori {
  type ItemSet = Set[String]
  case class Item(set: ItemSet, support: Int)
  type Transaction = List[ItemSet]
  
  val dataFIle = "data/mydata.txt"

  def main(args: Array[String]) = {
    println("Please enter min support : ")
    val support = scala.io.StdIn.readLine().toDouble
      
    println("Please enter the desired data : ")
    val writer=new PrintWriter(new File(dataFIle))  
    var lines:Array[String] = Array()
    var text = ""
    breakable {
        while (true) {
          var line = scala.io.StdIn.readLine()
          if(line != ""){
              text += line+"\n"
          }else{
              break
          }
        }
    }
    
    writer.write(text)
    writer.close
    
    getTransactions(dataFIle) match {
      case Success(set) => {
          println("Minimum support: " + support)
          val occuranceMap = set.flatten.foldLeft(Map[String,Int]() withDefaultValue 0) {
            (m,x) => m + (x -> (1 + m(x)))
          }
      
          val noOfRows = (set.size * support).toDouble
          val currentSet = occuranceMap.filter(item => item._2 >= noOfRows).toList
          val myItems = currentSet.map(tuple => Item(Set(tuple._1), tuple._2))

          println("Frequent ItemSets:\n" + myItems + "\n")
      }
      case Failure(e) => println(e.getMessage)
    }
  }

  def getTransactions(file: String): Try[Transaction] = Try {
    Source.fromFile(file)
      .getLines()
      .map(_.split(" ").toSet)
      .toList
  }

}
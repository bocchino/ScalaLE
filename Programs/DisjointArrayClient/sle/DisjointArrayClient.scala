import sle.annotations._
import sle.containers._

/**
 * Sample client for DisjointArray
 */
object DisjointArrayClient {

  @params("R")
  class CellArray(size:Int,factory:Factory[Cell]) 
  extends (DisjointArray[Cell] @args("R"))(size,factory)

  def main(args:Array[String]) {
    val Cells = new Region
    val arr = new (CellArray @args(Cells))(10, cellFactory)
    arr.swap(0,9)
    val r = new Region
    arr.set(5, new (Cell @args("Cells::r"))(42))
    arr.parApply(cellIncrement)
  }

}

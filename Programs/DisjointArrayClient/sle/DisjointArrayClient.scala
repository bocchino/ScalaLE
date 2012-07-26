import sle.annotations._
import sle.containers._

import Cell._

/**
 * Sample client for DisjointArray
 */
object DisjointArrayClient {

  @params("R")
  class CellArray(size:Int)
  extends (DisjointArray[Cell] @args("R"))(size,Cell.factory)

  def main(args:Array[String]) {
    val Cells = new Region
    val arr = new (CellArray @args(Cells))(10)
    arr.swap(0,9)
    val r = new Region
    arr(5) = new (Cell @args("Cells::r"))(42)
    arr parApply { _.data += 1 }
  }

}

import sle.annotations._
import sle.containers._

@params("R")
class Cell(var data:Int) 

object Cell {
  @params("R1")
  @effect(none)
  def factory(i:Int):Cell @args("R1") = {
    new (Cell @args("R1"))(i)
  }
}


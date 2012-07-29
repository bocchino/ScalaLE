import sle.annotations._
import sle.containers._

@params("R")
class Cell(var data:Int) 

object Cell {
  @params("Rf")
  @effect(none)
  def factory(i:Int):Cell @args("Rf") = {
    new (Cell @args("Rf"))(i)
  }
}


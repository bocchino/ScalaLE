import sle.annotations._
import sle.containers._

@params("R")
class Cell(var data:Int)

object cellFactory extends Factory[Cell] {
  // Default effect comes from superclass, i.e., 'writes R'
  @params("R")
  override def create(i:Int) = new (Cell @args("R"))(i)
}

object cellIncrement extends ParOp[Cell] { // Default effect argument is 'pure'
  // Default effect comes from superclass, i.e., 'writes R; E' with E=pure
  @params("R")
  override def op(cell:(Cell @args("R"))) {
    cell.data += 1
  }
}

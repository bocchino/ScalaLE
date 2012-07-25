package sle.containers

import sle.annotations._

trait Factory[@params("_") T] {
  @params("R")
  @effect(writes("R"))
  def create(i:Int):T @args("R")
}

@params("E": @Effect)
trait ParOp[@params("_") T] {
  @params("R")
  @effect(writes("R")+"E")
  def op(elt:T @args("R")):Unit
}


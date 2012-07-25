package sle.containers

import sle.annotations._

@params("R")
abstract class IndexParameterizedArray[T](val size:Int) {
  val rep = (new Array[AnyRef](size)).asInstanceOf[Array[T @args("R")]]
  def apply(i:Int):T @args("R") = rep(i)
  def update(i:Int,elt:T @args("R")) {
    rep(i) = elt
  }
}

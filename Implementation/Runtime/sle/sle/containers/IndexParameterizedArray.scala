package sle.containers

import sle.annotations._

@params("R")
abstract class IndexParameterizedArray[@params("_") T](val size:Int) {
  val rep = (new Array[AnyRef](size)).asInstanceOf[Array[T @args("R::_")]]

  @effect(assume(reads("R::(i)")))
  def apply(i:Int):T @args("R") = rep(i)

  @effect(assume(writes("R::(i)")))
  def update(i:Int,elt:T @args("R::_")) {
    rep(i) = elt
  }
}

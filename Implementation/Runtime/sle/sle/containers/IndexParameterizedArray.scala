package sle.containers

import sle.annotations._

@params("Rep","Elt")
abstract class IndexParameterizedArray[@params("_") T](val size:Int) {
  val rep = (new Array[AnyRef](size)).asInstanceOf[Array[T @args("Elt::_")]]

  @effect(assume(reads("R::Rep::(i)")))
  def apply(i:Int):T @args("Elt::_") = rep(i)

  @effect(assume(writes("R::Rep::(i)")))
  def update(i:Int,elt:T @args("Elt::_")) {
    rep(i) = elt
  }
}

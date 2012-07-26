package sle.containers

import sle.annotations._

@params("R")
abstract class IndexParameterizedArray[T](val size:Int) {
  val constructorEffect = none

  val rep = (new Array[Any](size)).asInstanceOf[Array[T]]

  @effect(assume(reads("R::(i)")))
  def apply(i:Int):T = rep(i)

  @effect(assume(writes("R::(i)")))
  def update(i:Int,elt:T) {
    rep(i) = elt
  }
}

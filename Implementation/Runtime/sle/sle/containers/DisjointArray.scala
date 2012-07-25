package sle.containers

import sle.annotations._

@params("R")
@(constructor @precondition("true") @effect(writes("R::_")))
@(default @invariant("this isValid"))
class DisjointArray[@params("_") T <: AnyRef](size:Int, 
					      factory:Factory[T])
{
  val Rep = new Region

  class RepArray(size:Int) 
  extends (IndexParameterizedArray[T] @args("R::_","R::Rep::(this.index)"))(size) {
    @effect(reads("R::Rep::(start,end)"))
    @predicate def isValidInterval(S:Set, start:Int, end:Int):Boolean = {
      (start >= end) ||
      existsRegion (R1 => {
	existsSet (S1 => {
	  (this isValidInterval(S1,start+1,end)) &&
	  (this(start).hasType[T @args(R1)]) &&
	  ((R1 union S1) in S) &&
	  (R1 disjoint S1)
	})
      })
    }
  }

  private val rep = new RepArray(size);
  for (i <- 0 to size)
  {
    val r = new Region
    rep(i) = factory.create(i): @args("R::r")
  }

  @effect(reads("R::Rep::(_)"))
  @predicate def isValid:Boolean = {
    existsSet(S => this isValidWRT(S))
  }

  @effect(reads("R::Rep::(_)"))
  @predicate def isValidWRT(S:Set):Boolean =
  {
    (this.rep isValidInterval(S,0,this.rep.size)) &&
    (S in "R::_") &&
    (S disjoint "R::Rep::(_)")
  }
  
  @effect(reads("R::Rep::(_)"))
  @predicate def setPrecondition(R1:Region):Boolean = {
    (R1 in "R::_") &&
    existsSet (S => {
      (this isValidWRT(S)) &&
      (R1 disjoint (S union "R::Rep::(_)"))
    })
  }

  @params("R1")
  @precondition("setPrecondition(R1)")
  @effect(writes("R::Rep::(i)"))
  def set(i:Int, elt:T @args("R1")) 
  {
    rep(i) = elt.asInstanceOf[T @args("R::_")]
  }

  @effect(reads("R::Rep::(i)"))
  def get(i:Int):T @args("R::_") = rep(i)

  @effect(writes("R::Rep::(i)","R::Rep::(j)"))
  def swap(i:Int, j:Int)
  {
    val t = this.rep(i)
    this.rep(j) = this.rep(i)
    this.rep(i) = t
  }

  @params("E": @Effect)
  @effect("writes R::_","E")
  def parApply(operation:ParOp[T] @args("E"))
  {
    (0 to rep.size).par foreach {
      i => operation.op(rep(i))
    }
  }
}

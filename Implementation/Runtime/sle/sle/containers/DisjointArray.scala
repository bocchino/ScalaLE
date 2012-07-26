package sle.containers

import sle.annotations._

@params("R","E": @Effect)
class DisjointArray[@params("_") T](size:Int, 
				    factory:(Int => T @args("R1")) @params("R1") @effect("E"))
{
  val defaultInvariant = this isValid
  val defaultEffect = reads("R::Rep::(_)")
  
  val constructorPrecondition = true
  val constructorEffect = writes("R::_")+"E"

  val Rep = new Region

  class RepArray(size:Int) 
    extends (IndexParameterizedArray[T @args("R::_")] @args("R::Rep"))(size)
  {
    @effect(reads("R::Rep::(start,end)"))
    @predicate def isValidInterval(S:RegionSet, start:Int, end:Int):Boolean = {
      (start >= end) ||
      existsRegion (R1 => {
	existsRegionSet (S1 => {
	  (this isValidInterval(S1,start+1,end)) &&
	  (this(start).hasType[T @args(R1)]) &&
	  ((R1 + S1) in S) &&
	  (R1 disjoint S1)
	})
      })
    }
  }

  private val rep = new RepArray(size);
  for (i <- 0 to size)
  {
    val r = new Region
    rep(i) = factory(i): @args("R::r")
  }

  @predicate def isValid:Boolean = {
    existsRegionSet(S => this isValidWRT(S))
  }

  @predicate def isValidWRT(S:RegionSet):Boolean =
  {
    (this.rep isValidInterval(S,0,this.rep.size)) &&
    (S in "R::_") &&
    (S disjoint "R::Rep::(_)")
  }
  
  @params("R1")
  @precondition(
    (Region("R1") in "R::_") &&
    existsRegionSet (S => {
      (this isValidWRT(S)) &&
      (Region("R1") disjoint (S + "R::Rep::(_)"))
    })
  )
  @effect(writes("R::Rep::(i)"))
  def update(i:Int,elt:T @args("R1")) {
    rep(i) = elt.asInstanceOf[T @args("R::_")]
  }

  @effect(reads("R::Rep::(i)"))
  def apply(i:Int):T @args("R::_") = rep(i)

  @effect(writes("R::Rep::(i)"+"R::Rep::(j)"))
  def swap(i:Int, j:Int)
  {
    val t = this.rep(i)
    this.rep(j) = this.rep(i)
    this.rep(i) = t
  }

  type opType = (T @args("R1") => Unit) @params("R1","E": @Effect) @effect(writes("R1")+"E")

  @params("E": @Effect)
  @effect(writes("R::_")+"E")
  @invariant(defaultInvariant && ((writes("R::_")+"E") | "E"))
  def parApply(op:opType)
  {
    (0 to rep.size).par foreach {
      i => op(rep(i))
    }
  }

}

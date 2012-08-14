package sle.containers

import sle.annotations._

@params("R","E": @Effect)
class DisjointArray[@params("_") T](size:Int, 
				    factory:(Int => T @args("Rf")) @params("Rf") @effect("E"))
{
  val defaultInvariant = this isValid
  val defaultEffect = reads("R::Rep::(_)")
  
  val constructorPrecondition = true
  val constructorEffect = writes("R::_")+"E"

  val Rep = new Region

  @effect(none)
  @predicate def inRange(i:Int,start:Int,end:Int):Boolean = {
    (i >= start) && (i <= end)
  }

  class RepArray(size:Int) 
    extends (IndexParameterizedArray[T @args("R::_")] @args("R::Rep"))(size)
  {
    @effect(reads("R::Rep::(start,end)"))
    @predicate def isValidInterval(S:RegionSet, start:Int, end:Int):Boolean = {
      (S in "R::_") && 
      (S disjoint "R::Rep::(_)") &&
      forAll((i:Int,j:Int) => {
	(inRange(i,start,end) && inRange(j,start,end)) -> {
	  exists((Ri:Region,Rj:Region) => {
	    this(i).hasType[T @args(Ri)] &&
	    (Ri in S) &&
	    this(j).hasType[T @args(Rj)] &&
	    (Rj in S) &&
	    ((i != j) -> (Ri disjoint Rj))
	  })
        }
      })
    }
  }

  private val rep = new RepArray(size);
  for (i <- 0 to size)
  {
    invariant(exists((S:RegionSet) => rep isValidInterval(S,0,i-1)))    

    assertion {
      exists ((S:RegionSet) => {
	(rep isValidInterval(S,0,i-1)) &&
	("R::FRESH" disjoint "R::Rep::(_)") &&
	("R::FRESH" disjoint S)
      })
    }
	   
    val r = new Region

    assertion {
      exists ((S:RegionSet,Ri:Region) => {
	(rep isValidInterval(S,0,i-1)) &&
	("R::r" in "R::_") && 
	("R::r" disjoint "R::Rep::(_)") &&
	((factory(i): @args("R::r")).hasType[T @args("R::r")]) &&
	("R::r" disjoint S)
      })
    }
	   
    assertion {
      exists ((S:RegionSet,Ri:Region) => {
	(rep isValidInterval(S,0,i-1)) &&
	(Ri in "R::_") && 
	(Ri disjoint "R::Rep::(_)") &&
	((factory(i): @args("R::r")).hasType[T @args(Ri)]) &&
	(Ri disjoint S)
      })
    }
	   
    rep(i) = factory(i): @args("R::r")

    assertion {
      exists ((S:RegionSet,Ri:Region) => {
	(rep isValidInterval(S,0,i-1)) &&
	(Ri in "R::_") && 
	(Ri disjoint "R::Rep::(_)") &&
	(rep(i).hasType[T @args(Ri)]) &&
	(Ri disjoint S)
      })
    }
	   
    assertion {
       exists((S:RegionSet) => rep isValidInterval(S,0,i))
    }
  
  }


  @predicate def isValid:Boolean = {
    existsRegionSet(S => this isValidWRT(S))
  }

  @predicate def isValidWRT(S:RegionSet):Boolean =
  {
    this.rep isValidInterval(S,0,this.rep.size)
  }
  
  @params("Rf")
  @precondition(
    (Region("Rf") in "R::_") &&
    existsRegionSet (S => {
      (this isValidWRT(S)) &&
      (Region("Rf") disjoint (S + "R::Rep::(_)"))
    })
  )
  @effect(writes("R::Rep::(i)"))
  def update(i:Int,elt:T @args("Rf")) {
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

  type opType = (T @args("Rf") => Unit) @params("Rf","E": @Effect) @effect(writes("Rf")+"E")

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

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

  class RepArray(size:Int) 
    extends (IndexParameterizedArray[T @args("R::_")] @args("R::Rep"))(size)
  {
    @effect(reads("R::Rep::(start,end)"))
    @predicate def isValidInterval(S:RegionSet, start:Int, end:Int):Boolean = {
      (S in "R::_") && (S disjoint "R::Rep::(_)") && (
	(start > end) ||
	existsRegion (R1 => {
	  existsRegionSet (S1 => {
	    (this isValidInterval(S1,start,end-1)) &&
	    (this(end).hasType[T @args(R1)]) &&
	    ((R1 + S1) in S) &&
	    (R1 disjoint S1)
	  })
	})
      )
    }
  }

  private val rep = new RepArray(size);
  for (i <- 0 to size)
  {
    invariant(i >= 0 && existsRegionSet(S => rep isValidInterval(S,0,i-1)))    

    assertion {
      (i >= 0) && 
      existsRegionSet(S =>
	(S in "R::_") && (S disjoint "R::Rep::(_)") &&
        (rep isValidInterval(S,0,i-1))
      )
    } because "Definition of isValidInterval"

    assertion {
      (i >= 0) &&
      existsRegionSet(S =>
	(S in "R::_") && (S disjoint "R::Rep::(_)") &&
        existsRegionSet(S1 =>
	  (rep isValidInterval(S1,0,i-1)) &&
          (S1 in S)
        )
      )
    } because "Choose S1=S"

    assertion {
      (i >= 0) && existsRegionSet(S =>
        (S+"R::FRESH" in "R::_") && (S+"R::FRESH" disjoint "R::Rep::(_)") &&
        existsRegionSet (S1 =>
          (rep isValidInterval(S1,0,i-1)) &&
	  ((factory(i): @args("R::FRESH")).hasType[T @args("R::FRESH")]) &&
	  (("R::FRESH"+S1) in (S+"R::FRESH")) &&
          ("R::FRESH" disjoint S1)
        )
      )
    } because "RPL and set axioms"

    assertion {
      (i >= 0) && existsRegionSet(S =>
        (S in "R::_") && (S disjoint "R::Rep::(_)") &&
        existsRegionSet (S1 =>
          (rep isValidInterval(S1,0,i-1)) &&
          ((factory(i): @args("R::FRESH")).hasType[T @args("R::FRESH")]) &&
          (("R::FRESH" + S1) in S) &&
          ("R::FRESH" disjoint S1)
        )
      )
    } because "Renaming"

    assertion {
      (i >= 0) && existsRegionSet(S =>
        (S in "R::_") && (S disjoint "R::Rep::(_)") &&
          existsRegionSet (S1 =>
            existsRegion (R1 => 
              (rep isValidInterval(S1,0,i-1)) &&
              ((factory(i): @args("R::FRESH")).hasType[T @args(R1)]) &&
              ((R1 + S1) in S) &&
              (R1 disjoint S1)
            )
          )
        )
    } because "Choose R1=R::FRESH"

    val r = new Region

    assertion {
      (i >= 0) && existsRegionSet(S =>
	(S in "R::_") && (S disjoint "R::Rep::(_)") &&
          existsRegionSet (S1 =>
	    existsRegion (R1 =>
              ((factory(i): @args("R::r")).hasType[T @args(R1)]) &&
              ((R1 + S1) in S) &&
              (R1 disjoint S1)
            )
          )
        )
    } because "r is fresh"

    rep(i) = factory(i): @args("R::r")

    assertion {
      (i >= 0) && existsRegionSet(S =>
        (S in "R::_") && (S disjoint "R::Rep::(_)") && (
          existsRegionSet (S1 =>
            existsRegion (R1 =>
              (rep isValidInterval(S1,0,i-1)) &&
              (rep(i).hasType[T @args(R1)]) &&
              ((R1 + S1) in S) &&
              (R1 disjoint S1)
            )
          )
        )
      )
    } because "Assignment"
      
    assertion {
      (i >= 0) && existsRegionSet(S => rep isValidInterval(S,0,i))
    } because "Definition of isValidInterval"
  
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

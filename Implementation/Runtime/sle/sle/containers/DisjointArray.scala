package sle.containers

import sle.annotations._

/**
 * A class representing a disjoint array.
 *
 * @param T[_] The type of an array element. The type has a "hole" that
 *             can be filled in by a region binding.  For example, if Data[_]
 *             is bound to T[_], then T[R] is Data[R].
 * @param R    The region of the array
 * @param E    The effect of the user-specified operations
 *
 * The primary constructor takes a size parameter and a factory parameter.
 * The factory parameter has a function type that takes Int to T[Rf], for some
 * region Rf specified when the function is invoked.  Its effect is E,
 * the effect specified when the class DisjointArray is instantiated.
 */
@params("R","E": @Effect)
class DisjointArray[@params("_") T](size:Int, 
				    factory:(Int => T @args("Rf")) @params("Rf") @effect("E"))
{

  /**
   * Rep is a name for the region of the array representation.
   */
  val Rep = new Region

  /**
   * The default invariant is to assert that 'this' is a valid
   * DisjointArray.  This invariant is implicitly included in the
   * precondition (resp. postcondition) of any method that doesn't
   * specify an explicit precondition (resp. postcondition).
   */
  val defaultInvariant = this isValid

  /**
   * The default effect is to read the array representation, i.e.,
   * array regions of the form R::Rep::(i).
   */
  val defaultEffect = reads("R::Rep::(_)")
  
  /**
   * The predicate inRange says that an integer i is located
   * within the bounds [start,end].
   */
  @effect(none)
  @predicate def inRange(i:Int,start:Int,end:Int):Boolean = {
    (i >= start) && (i <= end)
  }

  /**
   * A class for the representation of the array.  It is an index
   * parameterized array such that each cell:
   * 
   * 1. has type T[R::_], i.e., its type is T[R'] for
   *    some R' in R::_.
   *
   * 2. is located in region R::Rep.
   */
  class RepArray(size:Int) 
    extends (IndexParameterizedArray[T @args("R::_")] @args("R::Rep"))(size)
  {
    @effect(reads("R::Rep::(start,end)"))
    /**
     * The predicate isValidInterval says that the interval [start,end]
     * defines a valid interval of the array with respect to the set S
     * of regions associated with the contained objects.
     */
    @predicate def isValidInterval(S:RegionSet, start:Int, end:Int):Boolean = {
      // S must be contained in R::_
      (S in "R::_") && 
      // S must be disjoint from the representation
      (S disjoint "R::Rep::(_)") &&
      // For all integers i and j that are in the range [start,end]
      forAll((i:Int,j:Int) => {
	(inRange(i,start,end) && inRange(j,start,end)) -> {
	  // There exist regions Ri and Rj such that
	  exists((Ri:Region,Rj:Region) => {
	    // Ri is in S and the ith array element has type T[Ri]
	    (Ri in S) && this(i).hasType[T @args(Ri)]
	    // Rj is in S and the jth array element has type T[Rj]
	    (Rj in S)&& this(j).hasType[T @args(Rj)] &&
	    // If i =/= j, then Ri and Rj are disjoint
	    ((i != j) -> (Ri disjoint Rj))
	  })
        }
      })
    }
  }

  /**
   * The primary constructor's precondition is 'true'.  This overrides
   * the default precondition.  Its postcondition is the default.
   */
  val constructorPrecondition = true

  /**
   * The primary constructor's effect is to write regions under R
   * and do the effect E.  This overrides the default effect.
   * */
  val constructorEffect = writes("R::_")+"E"

  /**
   * Create and initialize the array in the primary constructor.
   */
  private val rep = new RepArray(size);
  for (i <- 0 to size)
  {
    // TO PROVE:
    // Loop invariant:  there exists a set of regions S such that
    // 'rep' is a valid array in the interval [0,i-1]
    invariant(exists((S:RegionSet) => rep isValidInterval(S,0,i-1)))    

    assertion {
      exists ((S:RegionSet) => {
	(rep isValidInterval(S,0,i-1)) &&
	("R::FRESH" disjoint "R::Rep::(_)") &&
	("R::FRESH" disjoint S)
      })
    }

    // Create a fresh region r
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
	   
    // Do the assignment.  The invariant holds because
    // 1. It holds by assumption for [0,i-1] and
    // 2. We're assigning an object of type T[R::r], so we
    //    can extend S to include T::r and still be in R::_ and
    // 3. r is fresh, so the disjointness requirements are
    //    satisfied.
    rep(i) = factory(i): @args("R::r")

    assertion {
      exists ((S:RegionSet,Ri:Region) => {
	(rep isValidInterval(S,0,i-1)) &&
	(rep(i).hasType[T @args(Ri)]) &&
	(Ri in "R::_") && 
	(Ri disjoint "R::Rep::(_)") &&
	(Ri disjoint S)
      })
    }
	   
    assertion {
       exists((S:RegionSet) => rep isValidInterval(S,0,i))
    }
  
  }
  // TO PROVE: implied postcondition 'this isValid'

  /**
   * 'this' is a valid DisjointArray if there exists a region set
   * S such that 'this' is valid with respect to S.
   */
  @predicate def isValid:Boolean = {
    existsRegionSet(S => this isValidWRT(S))
  }

  /**
   * 'this' is valid with respect to S if 'this' if [0,rep.size]
   * defines a valid interval of the array with respect to S.
   */
  @predicate def isValidWRT(S:RegionSet):Boolean =
  {
    this.rep isValidInterval(S,0,this.rep.size)
  }

  /**
   * Assign 'elt' into element i of the array.  elt must have type
   * T[Rf], where Rf is some region satisfying the precondition.
   * 
   * The precondition says (1) Rf is included in R::_; and (2)
   * there exists a region set S such that (a) 'this' is valid
   * with respect to S (i.e., S contains the regions of 'this'
   * and those regions satisfy the disjointness requirements) and
   * (b) Rf is disjoint from S and from the representation of the
   * array.
   */
  @params("Rf")
  @precondition(
    (Region("Rf") in "R::_") &&
    existsRegionSet (S => {
      (this isValidWRT(S)) &&
      (Region("Rf") disjoint (S + "R::Rep::(_)"))
    })
  )
  /**
   * The effect of 'update' is to write cell i of the representation.
   */
  @effect(writes("R::Rep::(i)"))
  def update(i:Int,elt:T @args("Rf")) {
    rep(i) = elt.asInstanceOf[T @args("R::_")]
  }
  // TO PROVE:  Implied postcondition 'this isValid'

  @effect(reads("R::Rep::(i)"))
  def apply(i:Int):T @args("R::_") = rep(i)

  @effect(writes("R::Rep::(i)"+"R::Rep::(j)"))
  def swap(i:Int, j:Int)
  {
    val t = this.rep(i)
    this.rep(j) = this.rep(i)
    this.rep(i) = t
  }

  /**
   * The type of an operation. The type has a region parameter Rf and
   * an effect parameter E.  It defines a function from T[Rf] to Unit
   * that writes Rf and does effect E.
   */
  type opType = (T @args("Rf") => Unit) @params("Rf","E": @Effect) @effect(writes("Rf")+"E")

  /**
   * Apply a user-defined function in parallel to the array elements.
   * This method has an effect parameter E.  The invariant of the function
   * (i.e., precondition and postcondition) says that this is valid
   * (default invariant) AND that the effect 'writes R::_ + E' can be
   * done in parallel with E.
   */
  @params("E": @Effect)
  @invariant(defaultInvariant && ((writes("R::_")+"E") | "E"))
  @effect(writes("R::_")+"E")
  def parApply(op:opType)
  {
    (0 to rep.size).par foreach {
      i => op(rep(i))
    }
  }

}

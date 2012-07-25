/**
 * Scala LE Annotations
 * @author Rob Bocchino
 */

package sle

import annotation._

object annotations {
  /**
   * Regions
   */
  class Region extends Annotation {
    def in(arg:Any):Boolean = true
    def disjoint(arg:Any):Boolean = true
    def union(arg:Any):Set = new Set
  }

  implicit def stringToRegion(s:String) = 
    new Region

  /**
   * Region sets
   */
  class Set extends Annotation {
    def in(arg:Any):Boolean = true
    def disjoint(arg:Any):Boolean = true
    def union(arg:Any):Set = new Set
  }

  implicit def stringToSet(s:String) = 
    new Set

  /**
   * Effects
   */
  class Effect extends Annotation
  implicit def stringToEffect(s:String) = 
    new Effect

  class effect(e:Any*) extends Annotation
  def writes(arg:Any*) = new Effect
  def reads(arg:Any*) = new Effect
  def effect(e:Any*) = new Effect

  /**
   * Parameters and arguments to types and methods
   */
  class params(s:String*) extends Annotation
  class args(s:Any*) extends Annotation

  /**
   * Logical assertions
   */
  implicit def anyToTyped(a:Any) = new Typed
  class Typed {
    def hasType[T]:Boolean = true
  }

  def range(start:String,end:String):List[Int] = Nil

  class predicate extends Annotation

  class constructor extends Annotation
  class default extends Annotation

  class invariant(s:String) extends Annotation
  class precondition(s:String) extends Annotation

  def existsRegion(pred:(Region)=>Boolean):Boolean = true
  def existsSet(pred:(Set)=>Boolean):Boolean = true

}

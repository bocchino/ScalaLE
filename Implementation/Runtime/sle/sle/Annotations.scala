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
    def +(arg:Any):RegionSet = new RegionSet
  }

  def Region(s:String) = new Region
  implicit def stringToRegion(s:String) = 
    new Region

  /**
   * Region sets
   */
  class RegionSet extends Annotation {
    def in(arg:Any):Boolean = true
    def disjoint(arg:Any):Boolean = true
    def +(arg:Any):RegionSet = this
  }

  implicit def stringToRegionSet(s:String) = 
    new RegionSet

  /**
   * Effects
   */
  class Effect extends Annotation {
    def in(arg:Any):Boolean = true
    def |(arg:Any):Boolean = true
    def +(arg:Any):Effect = this
  }
  implicit def stringToEffect(s:String) = 
    new Effect

  class effect(e:Any) extends Annotation
  def writes(arg:Any) = new Effect
  def reads(arg:Any) = new Effect
  def effect(e:Any) = new Effect

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
  class invariant(pred:Any) extends Annotation
  class precondition(pred:Any) extends Annotation
  class default extends Annotation

  def existsRegion(pred:(Region)=>Boolean):Boolean = true
  def existsSet(pred:(RegionSet)=>Boolean):Boolean = true

}

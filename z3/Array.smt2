; Objects and types
; newObject : Int --> Object
(declare-datatypes () ((Object (newObject (id Int)))))
; newType : Int --> Type
(declare-datatypes () ((Type (newType (id Int)))))
; type : Object --> Type
(declare-fun type (Object) Type)

; Regions
(declare-datatypes () 
  (
    (Region 
      (regionName (index Int)) 
      (rpl (tail Region) (head Region))
    )
  )
)

; Region sets
(define-sort RegionSet () (Array Region Bool))
(declare-fun regionsUnder (Region) RegionSet)
(declare-fun arrayRegionsUnder (Region) RegionSet)

; Inclusion of region sets
(declare-fun included (RegionSet RegionSet) Bool)
(declare-fun elementOf (Region RegionSet) Bool)

; Disjointness of region sets
(declare-fun disjoint (RegionSet RegionSet) Bool)
(declare-fun disjoint (Region Region) Bool)
(declare-fun disjoint (Region RegionSet) Bool)
(assert 
  (forall ((R1 Region) (R2 Region)) 
    (=> 
      (disjoint R1 R2) 
      (disjoint R2 R1)
    )
  )
)

; Union of region sets
(declare-fun rsUnion (RegionSet RegionSet) RegionSet)

(assert 
  (forall ((S1 RegionSet) (S2 RegionSet) (S3 RegionSet))
    (=> 
      (and 
        (included S1 S3)
        (included S2 S3)
      ) 
      (included (rsUnion S1 S2) S3)
    )
  )
)

(assert 
  (forall ((S1 RegionSet) (S2 RegionSet) (S3 RegionSet))
    (=> 
      (and 
        (disjoint S1 S3)
        (disjoint S2 S3)
      ) 
      (disjoint (rsUnion S1 S2) S3)
    )
  )
)

(assert 
  (forall ((S1 RegionSet) (S2 RegionSet) (S3 RegionSet)) 
    (=> 
      (included S1 S3)
      (included S1 (rsUnion S2 S3))
    )
  )
)

(assert 
  (forall ((S1 RegionSet) (S2 RegionSet))
    (= (rsUnion S1 S2) (rsUnion S2 S1))
  )
)

; Adding a region to a region set
(declare-fun addRegion (Region RegionSet) RegionSet)

(assert 
  (forall ((R Region) (S1 RegionSet) (S2 RegionSet))
    (=> 
      (and 
        (elementOf R S2) 
        (included S1 S2)
      ) 
      (included (addRegion R S1) S2)
    )
  )
)

(assert 
  (forall ((R Region) (S1 RegionSet) (S2 RegionSet))
    (=> 
      (and 
        (disjoint R S2)
        (disjoint S1 S2)
      ) 
      (disjoint (addRegion R S1) S2)
    )
  )
)

(assert 
  (forall ((R1 Region) (R2 Region) (S RegionSet)) 
    (=> 
      (elementOf R1 S) 
      (elementOf R1 (addRegion R2 S))
    )
  )
)

(assert 
  (forall ((R Region) (S RegionSet)) 
    (elementOf R (addRegion R S))
  )
)

; Array-specific stuff
(declare-const R Region)
(declare-const Rep Int)
(declare-fun this (Int) Object)
(declare-fun T (Region) Type)

(define-fun inRange ((i Int) (start Int) (end Int)) Bool 
  (and (>= i start) (<= i end)))

(define-fun isValidInterval ((S RegionSet) (start Int) (end Int)) Bool
  (and
     ; S included in R::_
    (included S (regionsUnder R)) 

    ; S # R::Rep::(_)
    (disjoint S (arrayRegionsUnder (rpl R Rep)))

    (forall ((i Int) (j Int))
      (=>
        (and
	   (inRange i start end)
	   (inRange j start end)
	 )
	 (exists ((Ri Region) (Rj Region))
	   (and

	     ; type(this(i))=T[Ri]
	     (= 
	       (type (this i)) 
	       (T Ri)
	     )
	     (elementOf Ri S)

	     ; type(this(j))=T[Rj]
	     (= 
	       (type (this j)) 
	       (T Rj)
	     )
	     (elementOf Rj S)
	     
	     ; i =/= j => Ri # Rj
	     (=>
	       (not (= i j))
	       (disjoint Ri Rj)
	     )
	   )
        )
      )
    )
  )
)

; Verification conditions
(declare-const S RegionSet)
(declare-const i Int)
(declare-const Rnew Region)

; Precondition
(assert (isValidInterval S 0 i))

; Derived from code
(assert (elementOf Rnew (regionsUnder R)))
(assert (disjoint Rnew (arrayRegionsUnder (rpl R Rep))))
(assert (= (type (this (+ i 1))) (T Rnew)))
(assert (forall ((R Region)) (=> (elementOf R S) (disjoint Rnew R))))

; ~ Postcondition
(assert (not (isValidInterval (addRegion Rnew S) 0 (+ i 1))))

(check-sat)

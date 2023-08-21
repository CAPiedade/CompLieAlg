#
# CompLieAlg: CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct "Lie bracket" operations.
#
#! @Chapter Introduction
#!
#! CompLieAlg is a package which does some
#! interesting and cool things
#!
#! @Chapter Functionality
#!
#!
#! @Section Example Methods
#!
#! This section will describe the example
#! methods of CompLieAlg

#! @Description
#!   Insert documentation for your function here

#############################################################################
##
#C  IsSCAlgebraObj( <obj> )
#C  IsSCAlgebraObjCollection( <obj> )
#C  IsSCAlgebraObjFamily( <obj> )
##
##  S.~c. algebra elements may have inverses, in order to allow `One' and
##  `Inverse' we make them scalars.
##
DeclareCategory( "IsCompSCAlgebraObj", IsScalar );
DeclareCategoryCollections( "IsCompSCAlgebraObj" );
DeclareCategoryCollections( "IsCompSCAlgebraObjCollection" );
DeclareCategoryCollections( "IsCompSCAlgebraObjCollColl" );
DeclareCategoryFamily( "IsCompSCAlgebraObj" );


#############################################################################
##
#P  IsFullSCAlgebra( <A> )
##
##  An s.~c. algebra is a free left module $A$ over a ring-with-one $R$,
##  with multiplication defined on the vectors of the standard basis $B$
##  of an algebra $\hat{A}$ containing $A$ by the structure constants table
##  of $\hat{A}$.
##
##  $A$ is a *full s.~c. algebra* if it contains $B$.
##  (So a full s.~c. algebra need *not* contain the whole family of its
##  elements.)
#T Do we really need this in addition to `IsFullFPAlgebra',
#T or would it be misuse to take `IsFullFPAlgebra' here?
##
DeclareProperty( "IsCompFullSCAlgebra", IsFLMLOR and IsCompSCAlgebraObjCollection );


#############################################################################
##
#P  IsCanonicalBasisFullSCAlgebra( <B> )
##
##  is `true' if the underlying free left module of the basis <B> is a full
##  s.~c. algebra and <B> is equal to its canonical basis,
##  and `false' otherwise.
##
##  The canonical basis of a full s.~c. algebra consists of elements whose
##  external representations are standard basis vectors.
##
##  (The canonical basis of a full s.~c. algebra is constructed together with
##  the algebra.)
##
DeclareProperty( "IsCanonicalBasisCompFullSCAlgebra", IsBasis );

InstallTrueMethod( IsCanonicalBasis, IsCanonicalBasisCompFullSCAlgebra );

DeclareOperation( "P1", [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep, IsCompSCAlgebraObj and IsDenseCoeffVectorRep ]);
DeclareOperation( "P2", [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep, IsCompSCAlgebraObj and IsDenseCoeffVectorRep ]);
DeclareOperation( "OneOp1", [ IsCompSCAlgebraObj]);
DeclareOperation( "OneOp2", [ IsCompSCAlgebraObj]);
DeclareOperation( "InverseOp1", [ IsCompSCAlgebraObj]);
DeclareOperation( "InverseOp2", [ IsCompSCAlgebraObj]);
DeclareGlobalFunction("CompLieAlgByStructureConstants");
DeclareGlobalFunction("TestMixedJacobi");
DeclareOperation("IsCompLieAbelian", [IsCompFullSCAlgebra]);
DeclareOperation( "MutableBasisOfCompProductSpace", [IsCompFullSCAlgebra, IsCompFullSCAlgebra]);

#############################################################################
##
#F  IsSCAlgebraObjSpace( <V> )
##
##  If an $F$-vector space <V> is in the filter `IsSCAlgebraObjSpace' then
##  this expresses that <V> consists of elements in a s.c. algebra,
##  and that <V> can be handled via the mechanism of nice bases
##  (see~"Vector Spaces Handled By Nice Bases"), in the following way.
##  The `NiceFreeLeftModuleInfo' value of <V> is irrelevant,
##  and the `NiceVector' value of $v \in <V>$ is defined as
##  $`ExtRepOfObj'( v )$.
##
DeclareHandlingByNiceBasis( "IsCompSCAlgebraObjSpace",
    "for free left modules of s.c. algebra elements" );



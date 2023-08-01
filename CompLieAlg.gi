
#############################################################################
##
##  This file is part of GAP, a system for computational discrete algebra.
##  This file's authors include Thomas Breuer.
##
##  Copyright of GAP belongs to its developers, whose names are too numerous
##  to list here. Please refer to the COPYRIGHT file for details.
##
##  SPDX-License-Identifier: GPL-2.0-or-later
##
##  This file contains methods for elements of algebras given by structure
##  constants (s.~c.).
##
##  The family of s.~c. algebra elements has the following components.
##
##  `sctable' :
##        the structure constants table,
##  `names' :
##        list of names of the basis vectors (for printing only),
##  `zerocoeff' :
##        the zero coefficient (needed already for the s.~c. table),
##  `defaultTypeDenseCoeffVectorRep' :
##        the type of s.~c. algebra elements that are represented by
##        a dense list of coefficients.
##
##  If the family has *not* the category `IsFamilyOverFullCoefficientsFamily'
##  then it has the component `coefficientsDomain'.
##



#############################################################################
##
#M  IsWholeFamily( <V> )  . . . . . . . for s.~c. algebra elements collection
##
InstallMethod( IsWholeFamily,
    "for s. c. algebra elements collection",
    [ IsCompSCAlgebraObjCollection and IsLeftModule and IsFreeLeftModule ],
    function( V )
    local Fam;
    Fam:= ElementsFamily( FamilyObj( V ) );
    if IsFamilyOverFullCoefficientsFamily( Fam ) then
      return     IsWholeFamily( LeftActingDomain( V ) )
             and IsCompFullSCAlgebra( V );
    else
      return     LeftActingDomain( V ) = Fam!.coefficientsDomain
             and IsCompFullSCAlgebra( V );
    fi;
    end );


#############################################################################
##
#M  IsCompFullSCAlgebra( <V> )  . . . . . . for s.~c. algebra elements collection
##
InstallMethod( IsCompFullSCAlgebra,
    "for s. c. algebra elements collection",
    [ IsCompSCAlgebraObjCollection and IsAlgebra ],
    V -> Dimension(V) = Length( ElementsFamily( FamilyObj( V ) )!.names ) );


#############################################################################
##
#R  IsDenseCoeffVectorRep( <obj> )
##
##  This representation uses a coefficients vector
##  w.r.t. the basis that is known for the whole family.
##
##  The external representation is the coefficients vector,
##  which is stored at position 1 in the object.
##
#if IsHPCGAP then
#DeclareRepresentation( "IsDenseCoeffVectorRep",
#    IsAtomicPositionalObjectRep, [ 1 ] );
#else
#DeclareRepresentation( "IsDenseCoeffVectorRep",
#    IsPositionalObjectRep, [ 1 ] );
#fi;


#############################################################################
##
#M  ObjByExtRep( <Fam>, <descr> ) . . . . . . . .  for s.~c. algebra elements
##
##  Check whether the coefficients list <coeffs> has the right length,
##  and lies in the correct family.
##  If the coefficients family of <Fam> has a uniquely determined zero
##  element, we need to check only whether the family of <descr> is the
##  collections family of the coefficients family of <Fam>.
##
InstallMethod( ObjByExtRep,
    "for s. c. algebra elements family",
    [ IsCompSCAlgebraObjFamily, IsHomogeneousList ],
    function( Fam, coeffs )
    if    IsFamilyOverFullCoefficientsFamily( Fam )
       or not IsBound( Fam!.coefficientsDomain ) then
      TryNextMethod();
    elif Length( coeffs ) <> Length( Fam!.names ) then
      Error( "<coeffs> must be a list of length ", Length( Fam!.names ) );
    elif not ForAll( coeffs, c -> c in Fam!.coefficientsDomain ) then
      Error( "all in <coeffs> must lie in `<Fam>!.coefficientsDomain'" );
    fi;
    return Objectify( Fam!.defaultTypeDenseCoeffVectorRep,
                      [ Immutable( coeffs ) ] );
    end );

InstallMethod( ObjByExtRep,
    "for s. c. alg. elms. family with coefficients family",
    [ IsCompSCAlgebraObjFamily and IsFamilyOverFullCoefficientsFamily,
      IsHomogeneousList ],
    function( Fam, coeffs )
    if not IsIdenticalObj( CoefficientsFamily( Fam ),
                        ElementsFamily( FamilyObj( coeffs ) ) ) then
      Error( "family of <coeffs> does not fit to <Fam>" );
    elif Length( coeffs ) <> Length( Fam!.names ) then
      Error( "<coeffs> must be a list of length ", Length( Fam!.names ) );
    fi;
    return Objectify( Fam!.defaultTypeDenseCoeffVectorRep,
                      [ Immutable( coeffs ) ] );
    end );


#############################################################################
##
#M  ExtRepOfObj( <elm> )  . . . . . . . . . . . .  for s.~c. algebra elements
##
InstallMethod( ExtRepOfObj,
    "for s. c. algebra element in dense coeff. vector rep.",
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    elm -> elm![1] );


#############################################################################
##
#M  Print( <elm> )  . . . . . . . . . . . . . . .  for s.~c. algebra elements
##
InstallMethod( PrintObj,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( elm )

    local F,      # family of `elm'
          names,  # generators names
          len,    # dimension of the algebra
          zero,   # zero element of the ring
          depth,  # first nonzero position in coefficients list
          one,    # identity element of the ring
          i;      # loop over the coefficients list

    F     := FamilyObj( elm );
    names := F!.names;
    elm   := ExtRepOfObj( elm );
    len   := Length( elm );

    # Treat the case that the algebra is trivial.
    if len = 0 then
      Print( "<zero of trivial s.c. algebra>" );
      return;
    fi;

    depth := PositionNonZero( elm );

    if len < depth then

      # Print the zero element.
      # (Note that the unique element of a zero algebra has a name.)
      Print( "0*", names[1] );

    else

      one:= One(  elm[1] );
      zero:= Zero( elm[1] );

      if elm[ depth ] <> one then
        Print( "(", elm[ depth ], ")*" );
      fi;
      Print( names[ depth ] );

      for i in [ depth+1 .. len ] do
        if elm[i] <> zero then
          Print( "+" );
          if elm[i] <> one then
            Print( "(", elm[i], ")*" );
          fi;
          Print( names[i] );
        fi;
      od;

    fi;
    end );

#############################################################################
##
#M  String( <elm> )  . . . . . . . . . . . . . . .  for s.~c. algebra elements
##
InstallMethod( String,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( elm )

    local F,      # family of `elm'
          s,      # string
          names,  # generators names
          len,    # dimension of the algebra
          zero,   # zero element of the ring
          depth,  # first nonzero position in coefficients list
          one,    # identity element of the ring
          i;      # loop over the coefficients list

    F     := FamilyObj( elm );
    names := F!.names;
    elm   := ExtRepOfObj( elm );
    len   := Length( elm );

    # Treat the case that the algebra is trivial.
    if len = 0 then
      return "<zero of trivial s.c. algebra>";
    fi;

    depth := PositionNonZero( elm );

    s:="";
    if len < depth then

      # Print the zero element.
      # (Note that the unique element of a zero algebra has a name.)
      Append(s, "0*");
      Append(s,names[1]);

    else

      one:= One(  elm[1] );
      zero:= Zero( elm[1] );

      if elm[ depth ] <> one then
	Add(s,'(');
	Append(s,String(elm[ depth ]));
	Append(s, ")*" );
      fi;
      Append(s, names[ depth ] );

      for i in [ depth+1 .. len ] do
        if elm[i] <> zero then
          Add(s, '+' );
          if elm[i] <> one then
	    Add(s,'(');
	    Append(s,String(elm[ i ]));
	    Append(s, ")*" );
          fi;
	  Append(s, names[ i ] );
        fi;
      od;

    fi;
    return s;
    end );


#############################################################################
##
#M  One( <Fam> )
##
##  Compute the identity (if exists) from the s.~c. table.
##
InstallMethod( One,
    "for family of s. c. algebra elements",
    [ IsCompSCAlgebraObjFamily ],
    function( F )
    local one;
    one:= IdentityFromSCTable( F!.sctable );
    if one <> fail then
      one:= ObjByExtRep( F, one );
    fi;
    return one;
    end );


#############################################################################
##
#M  \=( <x>, <y> )  . . . . . . . . . . equality of two s.~c. algebra objects
#M  \<( <x>, <y> )  . . . . . . . . . comparison of two s.~c. algebra objects
#M  \+( <x>, <y> )  . . . . . . . . . . . .  sum of two s.~c. algebra objects
#M  \-( <x>, <y> )  . . . . . . . . . difference of two s.~c. algebra objects
#M  \*( <x>, <y> )  . . . . . . . . . .  product of two s.~c. algebra objects
#M  Zero( <x> ) . . . . . . . . . . . . . .  zero of an s.~c. algebra element
#M  AdditiveInverse( <x> )  . .  additive inverse of an s.~c. algebra element
#M  Inverse( <x> )  . . . . . . . . . . . inverse of an s.~c. algebra element
##
InstallMethod( \=,
    "for s. c. algebra elements",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj, IsCompSCAlgebraObj ],
    function( x, y ) return ExtRepOfObj( x ) = ExtRepOfObj( y ); end );

InstallMethod( \=,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y ) return x![1] = y![1]; end );

InstallMethod( \<,
    "for s. c. algebra elements",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj, IsCompSCAlgebraObj ],
    function( x, y ) return ExtRepOfObj( x ) < ExtRepOfObj( y ); end );

InstallMethod( \<,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ], 0,
    function( x, y ) return x![1] < y![1]; end );

InstallMethod( \+,
    "for s. c. algebra elements",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj, IsCompSCAlgebraObj ],
    function( x, y )
    return ObjByExtRep( FamilyObj(x), ExtRepOfObj(x) + ExtRepOfObj(y) );
    end );

InstallMethod( \+,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), x![1] + y![1] );
    end );

InstallMethod( \-,
    "for s. c. algebra elements",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj, IsCompSCAlgebraObj ],
    function( x, y )
    return ObjByExtRep( FamilyObj(x), ExtRepOfObj(x) - ExtRepOfObj(y) );
    end );

InstallMethod( \-,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), x![1] - y![1] );
    end );



InstallMethod( \*one,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    local F;
    F:= FamilyObj( x );
    return ObjByExtRep( F, SCTableProduct( F!.sctable1, x![1], y![1] ) );
    end );


InstallMethod( P1,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    local F;
    F:= FamilyObj( x );
    return ObjByExtRep( F, SCTableProduct( F!.sctable1, x![1], y![1] ) );
    end );

InstallMethod( P2,
    "for s. c. algebra elements in dense vector rep.",
    IsIdenticalObj,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep,
      IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    local F;
    F:= FamilyObj( x );
    return ObjByExtRep( F, SCTableProduct( F!.sctable2, x![1], y![1] ) );
    end );

InstallMethod( \*,
    "for ring element and s. c. algebra element",
    IsCoeffsElms,
    [ IsRingElement, IsCompSCAlgebraObj ],
    function( x, y )
    return ObjByExtRep( FamilyObj( y ), x * ExtRepOfObj( y ) );
    end );

InstallMethod( \*,
    "for ring element and s. c. algebra element in dense vector rep.",
    IsCoeffsElms,
    [ IsRingElement, IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    return ObjByExtRep( FamilyObj( y ), x * y![1] );
    end );

InstallMethod( \*,
    "for s. c. algebra element and ring element",
    IsElmsCoeffs,
    [ IsCompSCAlgebraObj, IsRingElement ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), ExtRepOfObj( x ) * y );
    end );

InstallMethod( \*,
    "for s. c. algebra element in dense vector rep. and ring element",
    IsElmsCoeffs,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep, IsRingElement ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), x![1] * y );
    end );

InstallMethod( \*,
    "for integer and s. c. algebra element",
    [ IsInt, IsCompSCAlgebraObj ],
    function( x, y )
    return ObjByExtRep( FamilyObj( y ), x * ExtRepOfObj( y ) );
    end );

InstallMethod( \*,
    "for integer and s. c. algebra element in dense vector rep.",
    [ IsInt, IsCompSCAlgebraObj and IsDenseCoeffVectorRep ],
    function( x, y )
    return ObjByExtRep( FamilyObj( y ), x * y![1] );
    end );

InstallMethod( \*,
    "for s. c. algebra element and integer",
    [ IsCompSCAlgebraObj, IsInt ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), ExtRepOfObj( x ) * y );
    end );

InstallMethod( \*,
    "for s. c. algebra element in dense vector rep. and integer",
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep, IsInt ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), x![1] * y );
    end );



InstallMethod( \/,
    "for s. c. algebra element and scalar",
    IsElmsCoeffs,
    [ IsCompSCAlgebraObj, IsScalar ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), ExtRepOfObj( x ) / y );
    end );

InstallMethod( \/,
    "for s. c. algebra element in dense vector rep. and scalar",
    IsElmsCoeffs,
    [ IsCompSCAlgebraObj and IsDenseCoeffVectorRep, IsScalar ],
    function( x, y )
    return ObjByExtRep( FamilyObj( x ), x![1] / y );
    end );

InstallMethod( ZeroOp,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    x -> ObjByExtRep( FamilyObj( x ), Zero( ExtRepOfObj( x ) ) ) );

InstallMethod( AdditiveInverseOp,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    x -> ObjByExtRep( FamilyObj( x ),
                      AdditiveInverse( ExtRepOfObj( x ) ) ) );

InstallOtherMethod( OneOp1,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( x )
    local F, one;
    F:= FamilyObj( x );
    one:= IdentityFromSCTable( F!.sctable1 );
    if one <> fail then
      one:= ObjByExtRep( F, one );
    fi;
    return one;
    end );

InstallOtherMethod( OneOp2,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( x )
    local F, one;
    F:= FamilyObj( x );
    one:= IdentityFromSCTable( F!.sctable2 );
    if one <> fail then
      one:= ObjByExtRep( F, one );
    fi;
    return one;
    end );

InstallOtherMethod( InverseOp1,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( x )
    local one, F;
    one:= One( x );
    if one <> fail then
      F:= FamilyObj( x );
      one:= QuotientFromSCTable( F!.sctable1, ExtRepOfObj( one ),
                                             ExtRepOfObj( x ) );
      if one <> fail then
        one:= ObjByExtRep( F, one );
      fi;
    fi;
    return one;
    end );

InstallOtherMethod( InverseOp2,
    "for s. c. algebra element",
    [ IsCompSCAlgebraObj ],
    function( x )
    local one, F;
    one:= One( x );
    if one <> fail then
      F:= FamilyObj( x );
      one:= QuotientFromSCTable( F!.sctable2, ExtRepOfObj( one ),
                                             ExtRepOfObj( x ) );
      if one <> fail then
        one:= ObjByExtRep( F, one );
      fi;
    fi;
    return one;
    end );

#############################################################################
##
#M  \in( <a>, <A> )
##
InstallMethod( \in,
    "for s. c. algebra element, and full s. c. algebra",
    IsElmsColls,
    [ IsCompSCAlgebraObj, IsCompFullSCAlgebra ],
    function( a, A )
    return IsSubset( LeftActingDomain( A ), ExtRepOfObj( a ) );
    end );


#############################################################################
##
#F  AlgebraByStructureConstants( <R>, <sctable1>, <sctable2> )
#F  AlgebraByStructureConstants( <R>, <sctable1>, <sctable2>, <name> )
##
##
##

BindGlobal( "CompAlgebraByStructureConstantsArg", function( arglist, filter )
    local T1,     # structure constants table 1
          T2,     # structure constants table 2
          n,      # dimensions of structure matrices
          R,      # coefficients ring
          zero,   # zero of `R'
          names,  # names of the algebra generators
          Fam,    # the family of algebra elements
          A,      # the algebra, result
          gens;   # algebra generators of `A'

    # Check the argument list.
    if not 2 < Length( arglist ) and IsRing( arglist[1] )
                                 and IsList( arglist[2] ) and IsList( arglist[3] ) then
      Error( "usage: AlgebraByStructureConstantsArg([<R>,<sctable1>,<sctable2>]) or \n",
             "AlgebraByStructureConstantsArg([<R>,<sctable1>,<sctable2>,<name>])" );
    fi;

    # Check the s.~c. table.
#T really do this?
    R    := arglist[1];
    zero := Zero( R );
    T1   := arglist[2];
    T2   := arglist[3];

    if zero = T1[ Length( T1 ) ] then
      T1:= Immutable( T1 );
    else
      if T1[ Length( T1 ) ] = 0 then
        T1:= ReducedSCTable( T1, One( zero ) );
      else
        Error( "<R> and <T1> are not compatible" );
      fi;
    fi;
    if zero = T2[ Length( T2 ) ] then
      T2:= Immutable( T2 );
    else
      if T2[ Length( T2 ) ] = 0 then
        T2:= ReducedSCTable( T2, One( zero ) );
      else
        Error( "<R> and <T2> are not compatible" );
      fi;
    fi;

    if T1[Length(T1)] <> T2[Length(T2)] then
        Error("<T1> and <T2> have distinct zeros");
    fi;

    if Length(T1) <> Length(T2) then
        Error( "<T1> and <T2> are not of the same dimension");
    fi;

    if Length( T1 ) = 2 then
      n:= 0;
    else
      n:= Length( T1[1] );
    fi;

    # Construct names of generators (used for printing only).
    if   Length( arglist ) = 3 then
      names:= List( [ 1 .. n ],
                    x -> Concatenation( "v.", String(x) ) );
      MakeImmutable( names );
    elif Length( arglist ) = 4 and IsString( arglist[4] ) then
      names:= List( [ 1 .. n ],
                    x -> Concatenation( arglist[3], String(x) ) );
      MakeImmutable( names );
    fi;

    # If the coefficients know to be additively commutative then
    # also the s.c. algebra will know this.
    if IsAdditivelyCommutativeElementFamily( FamilyObj( zero ) ) then
      filter:= filter and IsAdditivelyCommutativeElement;
    fi;

    # Construct the family of elements of our algebra.
    # If the elements family of `R' has a uniquely determined zero element,
    # then all coefficients in this family are admissible.
    # Otherwise only coefficients from `R' itself are allowed.
    Fam:= NewFamily( "CompSCAlgebraObjFamily", filter );
    if Zero( ElementsFamily( FamilyObj( R ) ) ) <> fail then
      SetFilterObj( Fam, IsFamilyOverFullCoefficientsFamily );
    else
      Fam!.coefficientsDomain:= R;
    fi;

    Fam!.sctable1  := T1;
    Fam!.sctable2  := T2;
    Fam!.names     := names;
    Fam!.zerocoeff := zero;

    # Construct the default type of the family.
    Fam!.defaultTypeDenseCoeffVectorRep :=
        NewType( Fam, IsCompSCAlgebraObj and IsDenseCoeffVectorRep );

    SetCharacteristic( Fam, Characteristic( R ) );
    SetCoefficientsFamily( Fam, ElementsFamily( FamilyObj( R ) ) );

    # Make the generators and the algebra.
    if 0 < n then
      SetZero( Fam, ObjByExtRep( Fam, List( [ 1 .. n ], x -> zero ) ) );
      gens:= Immutable( List( IdentityMat( n, R ),
                              x -> ObjByExtRep( Fam, x ) ) );
      A:= FLMLORByGenerators( R, gens );
      UseBasis( A, gens );
    else
      SetZero( Fam, ObjByExtRep( Fam, EmptyRowVector( FamilyObj(zero) ) ) );
      gens:= Immutable( [] );
      A:= FLMLORByGenerators( R, gens, Zero( Fam ) );
      SetIsTrivial( A, true );
      SetDimension( A, 0 );
    fi;
    Fam!.basisVectors:= gens;
#T where is this needed?

    # Store the algebra in the family of the elements,
    # for accessing the full algebra, e.g., in `DefaultFieldOfMatrixGroup'.
    Fam!.compFullSCAlgebra:= A;

    SetIsCompFullSCAlgebra( A, true );

    # Return the algebra.
    return A;
end );

InstallGlobalFunction( CompLieAlgByStructureConstants, function( arg )
    return CompAlgebraByStructureConstantsArg( arg, IsCompSCAlgebraObj );
end );




#############################################################################
##
#M  \.( <A>, <n> )  . . . . . . . access to generators of a full s.c. algebra
##
InstallAccessToGenerators( IsCompSCAlgebraObjCollection and IsCompFullSCAlgebra,
    "s.c. algebra containing the whole family",
    GeneratorsOfAlgebra );



#############################################################################
##
#M  NiceFreeLeftModuleInfo( <V> )
#M  NiceVector( <V>, <v> )
#M  UglyVector( <V>, <r> )
##
InstallHandlingByNiceBasis( "IsCompSCAlgebraObjSpace", rec(
    detect := function( R, gens, V, zero )
      return IsCompSCAlgebraObjCollection( V );
      end,

    NiceFreeLeftModuleInfo := ReturnTrue,

    NiceVector := function( V, v )
      return ExtRepOfObj( v );
      end,

    UglyVector := function( V, r )
      local F;
      F:= ElementsFamily( FamilyObj( V ) );
      if Length( r ) <> Length( F!.names ) then
        return fail;
      fi;
      return ObjByExtRep( F, r );
      end ) );


#############################################################################
##
#M  MutableBasis( <R>, <gens> )
#M  MutableBasis( <R>, <gens>, <zero> )
##
##  We choose a mutable basis that stores a mutable basis for a nice module.
##
InstallMethod( MutableBasis,
    "for ring and collection of s. c. algebra elements",
    [ IsRing, IsCompSCAlgebraObjCollection ],
    MutableBasisViaNiceMutableBasisMethod2 );

InstallOtherMethod( MutableBasis,
    "for ring, (possibly empty) list, and zero element",
    [ IsRing, IsList, IsCompSCAlgebraObj ],
    MutableBasisViaNiceMutableBasisMethod3 );


#############################################################################
##
#M  Coefficients( <B>, <v> )  . . . . . . coefficients w.r.t. canonical basis
##
InstallMethod( Coefficients,
    "for canonical basis of full s. c. algebra",
    IsCollsElms,
    [ IsBasis and IsCanonicalBasisCompFullSCAlgebra, IsCompSCAlgebraObj ],
    function( B, v )
    return ExtRepOfObj( v );
    end );


#############################################################################
##
#M  LinearCombination( <B>, <coeffs> )  . . . . . . . . . for canonical basis
##
InstallMethod( LinearCombination,
    "for canonical basis of full s. c. algebra",
    [ IsBasis and IsCanonicalBasisCompFullSCAlgebra, IsRowVector ],
    function( B, coeffs )
    return ObjByExtRep( ElementsFamily( FamilyObj( B ) ), coeffs );
    end );


#############################################################################
##
#M  BasisVectors( <B> ) . . . . . . for canonical basis of full s.~c. algebra
##
InstallMethod( BasisVectors,
    "for canonical basis of full s. c. algebra",
    [ IsBasis and IsCanonicalBasisCompFullSCAlgebra ],
    B -> ElementsFamily( FamilyObj(
             UnderlyingLeftModule( B ) ) )!.basisVectors );

InstallOtherMethod( BasisVectors,
    "for canonical basis of full s. c. algebra",
    [ IsCompSCAlgebraObjCollection and IsCompFullSCAlgebra ],
    A -> ElementsFamily( FamilyObj(A) )!.basisVectors );


#############################################################################
##
#M  Basis( <A> )  . . . . . . . . . . . . . . . basis of a full s.~c. algebra
##
InstallMethod( Basis,
    "for full s. c. algebra (delegate to `CanonicalBasis')",
    [ IsFreeLeftModule and IsCompSCAlgebraObjCollection and IsCompFullSCAlgebra ],
    CANONICAL_BASIS_FLAGS,
    CanonicalBasis );


#############################################################################
##
#M  CanonicalBasis( <A> ) . . . . . . . . . . . basis of a full s.~c. algebra
##
InstallMethod( CanonicalBasis,
    "for full s. c. algebras",
    [ IsFreeLeftModule and IsCompSCAlgebraObjCollection and IsCompFullSCAlgebra ],
    function( A )
    local B;
    B:= Objectify( NewType( FamilyObj( A ),
                                IsCanonicalBasisCompFullSCAlgebra
                            and IsAttributeStoringRep
                            and IsFiniteBasisDefault
                            and IsCanonicalBasis ),
                   rec() );
    SetUnderlyingLeftModule( B, A );
    SetStructureConstantsTable( B,
        ElementsFamily( FamilyObj( A ) )!.sctable );
    return B;
    end );


#############################################################################
##
#M  IsCanonicalBasisCompFullSCAlgebra( <B> )
##
InstallMethod( IsCanonicalBasisCompFullSCAlgebra,
    "for a basis",
    [ IsBasis ],
    function( B )
    local A;
    A:= UnderlyingLeftModule( B );
    return     IsCompSCAlgebraObjCollection( A )
           and IsCompFullSCAlgebra( A )
           and IsCanonicalBasis( B );
    end );

#T change implementation: bases of their own right, as for Gaussian row spaces,
#T if the algebra is Gaussian


#############################################################################
##
#M  Intersection2( <V>, <W> )
##
##  Contrary to the generic case that is handled by `Intersection2Spaces',
##  we know initially a (finite dimensional) common coefficient space,
##  so we can avoid the intermediate construction of such a space.
##
InstallMethod( Intersection2,
    "for two spaces in a common s.c. algebra",
    IsIdenticalObj,
    [ IsVectorSpace and IsCompSCAlgebraObjCollection,
      IsVectorSpace and IsCompSCAlgebraObjCollection ],
    function( V, W )
    local F,       # coefficients field
          gensV,   # list of generators of 'V'
          gensW,   # list of generators of 'W'
          Fam,     # family of an element
          inters;  # intersection, result

    F:= LeftActingDomain( V );
    if F <> LeftActingDomain( W ) then
      # The generic method is good enough for this.
      TryNextMethod();
    fi;

    gensV:= GeneratorsOfLeftModule( V );
    gensW:= GeneratorsOfLeftModule( W );
    if IsEmpty( gensV ) or IsEmpty( gensW ) then
      inters:= [];
    else
      gensV:= List( gensV, ExtRepOfObj );
      gensW:= List( gensW, ExtRepOfObj );
      if not (     ForAll( gensV, v -> IsSubset( F, v ) )
               and ForAll( gensW, v -> IsSubset( F, v ) ) ) then
        # We are not in a Gaussian situation.
        TryNextMethod();
      fi;
      Fam:= ElementsFamily( FamilyObj( V ) );
      inters:= List( SumIntersectionMat( gensV, gensW )[2],
                     x -> ObjByExtRep( Fam, x ) );
    fi;

    # Construct the intersection space, if possible with a parent,
    # and with as much structure as possible.
    if IsEmpty( inters ) then
      inters:= TrivialSubFLMLOR( V );
    elif IsFLMLOR( V ) and IsFLMLOR( W ) then
      inters:= FLMLOR( F, inters, "basis" );
    else
      inters:= VectorSpace( F, inters, "basis" );
    fi;
    if     HasParent( V ) and HasParent( W )
       and IsIdenticalObj( Parent( V ), Parent( W ) ) then
      SetParent( inters, Parent( V ) );
    fi;

    # Run implications by the subset relation.
    UseSubsetRelation( V, inters );
    UseSubsetRelation( W, inters );

    # Return the result.
    return inters;
    end );




VectorFormOfSCTableEntry := function(cij, n , zero)
  local Cij, l1, u ;
    Cij := List([1 .. n], u -> zero);
    for l1 in cij[1] do
      Cij[l1] := cij[2][Position(cij[1],l1)];
    od;
    return Cij;
  end;



#############################################################################
##
#F  TestMixedJacobi( <T> )
##
##  We check whether for all $1 \leq m \leq n$ the equality
##  $\sum_{l=1}^n c_{jkl} c_{ilm} + c_{kil} c_{jlm} + c_{ijl} c_{klm} = 0$
##  holds.
##
InstallGlobalFunction( TestMixedJacobi, function( T1,T2 )
    local zero,           # the zero of the field
          n,              # dimension of the algebra
          i, j, k, m,     # loop variables
          cij, cki, cjk,  # structure constant vectors
          dij, dki, djk,
          Cij, Cki, Cjk,
          Dij, Dki, Djk,
          TJT1, TJT2,
          sum,
          t,
          elm;

    
    if Length(T1) <> Length(T2) then
        Error("<T1> and <T2> are not of the same dimension");
    fi;
    if T1[Length(T1)] <> T2[Length(T2)] then
        Error("<T1> and <T2> have distinct zeros");
    fi;
    TJT1 := TestJacobi(T1);
    TJT2 := TestJacobi(T2);

    if not TJT1 = true then
      return [TJT1[1], TJT1[2], TJT1[3],TJT1 = true,TJT2 = true,false];
    elif not TJT1 = true then
      return [TJT2[1], TJT2[2], TJT2[3],TJT1 = true,TJT2 = true,false];
    fi;

    zero:= T1[ Length( T1 ) ];
    n:= Length( T1 ) - 2;

    for i in [ 1 .. n ] do
      for j in [ i+1 .. n ] do
        cij:= T1[i][j];
        dij:= T2[i][j];
        Cij := VectorFormOfSCTableEntry(cij, n , zero);
        Dij := VectorFormOfSCTableEntry(dij, n , zero);
        for k in [ j+1 .. n ] do
          cki:= T1[k][i];
          cjk:= T1[j][k];
          dki:= T2[k][i];
          djk:= T2[j][k];
          Cki := VectorFormOfSCTableEntry(cki, n , zero);
          Cjk := VectorFormOfSCTableEntry(cjk, n , zero);
          Dki := VectorFormOfSCTableEntry(dki, n , zero);
          Djk := VectorFormOfSCTableEntry(djk, n , zero);
          for m in [ 1 .. n ] do
            sum:= zero;
            for t in [ 1 .. n ] do
              sum:= sum + Cjk[t] * SCTableEntry( T2, i, t, m );
            od;
            for t in [ 1 .. n ] do
              sum:= sum + Cki[t] * SCTableEntry( T2, j, t, m );
            od;
            for t in [ 1 .. n ] do
              sum:= sum + Cij[t] * SCTableEntry( T2, k, t, m );
            od;
            for t in [ 1 .. n ] do
              sum:= sum + Djk[t] * SCTableEntry( T1, i, t, m );
            od;
            for t in [ 1 .. n ] do
              sum:= sum + Dki[t] * SCTableEntry( T1, j, t, m );
            od;
            for t in [ 1 .. n ] do
              sum:= sum + Dij[t] * SCTableEntry( T1, k, t, m );
            od;

            if sum <> zero then
              return [ i, j, k, true, true, false ];
            fi;
          od;
        od;
      od;
    od;

    return true;
end );


InstallMethod( IsCompLieAbelian,
    "for a Lie algebra with known basis",
    true,
    [ IsCompFullSCAlgebra ], 0,
    function( L )

    local T1,     # structure constants table 1
          T2,     # structure constants table 2
          i,      # loop variable
          j;      # loop variable

    T1 := ElementsFamily( FamilyObj(L) )!.sctable1;
    T2 := ElementsFamily( FamilyObj(L) )!.sctable2;
    for i in T1{ [ 1 .. Length( T1 ) - 2 ] } do
      for j in i do
        if not IsEmpty( j[1] ) then
          return false;
        fi;
      od;
    od;
    for i in T2{ [ 1 .. Length( T2 ) - 2 ] } do
      for j in i do
        if not IsEmpty( j[1] ) then
          return false;
        fi;
      od;
    od;
    return true;
    end );
#
# CompLieAlg: CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct "Lie bracket" operations.
#
# This file runs package tests. It is also referenced in the package
# metadata in PackageInfo.g.
#
LoadPackage( "CompLieAlg" );

TestDirectory(DirectoriesPackageLibrary( "CompLieAlg", "tst" ),
  rec(exitGAP := true));

FORCE_QUIT_GAP(1); # if we ever get here, there was an error

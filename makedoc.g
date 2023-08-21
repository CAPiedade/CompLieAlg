#
# CompLieAlg: CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct "Lie bracket" operations.
#
# This file is a script which compiles the package manual.
#
if fail = LoadPackage("AutoDoc", "2018.02.14") then
    Error("AutoDoc version 2018.02.14 or newer is required.");
fi;

AutoDoc( rec( scaffold := true, autodoc := true ) );

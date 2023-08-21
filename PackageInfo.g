#
# CompLieAlg: CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct "Lie bracket" operations.
#
# This file contains package meta data. For additional information on
# the meaning and correct usage of these fields, please consult the
# manual of the "Example" package as well as the comments in its
# PackageInfo.g file.
#
SetPackageInfo( rec(

PackageName := "CompLieAlg",
Subtitle := "CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct 'Lie bracket' operations.",
Version := "0.1",
Date := "21/08/2023", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    FirstNames := "Claudio Alexandre",
    LastName := "Piedade",
    WWWHome := "https://www.fc.up.pt/pessoas/claudio.piedade/",
    Email := "claudio.piedade@fc.up.pt",
    IsAuthor := true,
    IsMaintainer := true,
    #PostalAddress := TODO,
    Place := "Porto, Portugal",
    Institution := "Centro de Matemática da Universidade do Porto",
  ),
  rec(
    FirstNames := "Bernardo",
    LastName := "Cunha",
    #WWWHome := TODO,
    Email := "bernardo.mariz@rai.usc.gal",
    IsAuthor := true,
    IsMaintainer := true,
    #PostalAddress := TODO,
    Place := "Porto, Portugal",
    Institution := "Centro de Matemática da Universidade do Porto",
  ),
],

#SourceRepository := rec( Type := "TODO", URL := "URL" ),
#IssueTrackerURL := "TODO",
PackageWWWHome := "https://github.com/CAPiedade/CompLieAlg/",
PackageInfoURL := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
README_URL     := Concatenation( ~.PackageWWWHome, "README.md" ),
ArchiveURL     := Concatenation( ~.PackageWWWHome,
                                 "/", ~.PackageName, "-", ~.Version ),

ArchiveFormats := ".tar.gz",

##  Status information. Currently the following cases are recognized:
##    "accepted"      for successfully refereed packages
##    "submitted"     for packages submitted for the refereeing
##    "deposited"     for packages for which the GAP developers agreed
##                    to distribute them with the core GAP system
##    "dev"           for development versions of packages
##    "other"         for all other packages
##
Status := "dev",

AbstractHTML   :=  "",

PackageDoc := rec(
  BookName  := "CompLieAlg",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "CompLieAlg is a GAP Package created to compute compatible Lie Algebras, taking two distinct 'Lie bracket' operations.",
),

Dependencies := rec(
  GAP := ">= 4.11",
  NeededOtherPackages := [ ],
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ],
),

AvailabilityTest := ReturnTrue,

TestFile := "tst/testall.g",

#Keywords := [ "TODO" ],

));



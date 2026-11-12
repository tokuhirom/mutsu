use Test;
use experimental :rakuast;

# A BEGIN-time `$*R.resolve-lexical-constant` sees what a loaded module put in
# the importing unit: a package it vivified under GLOBAL, and a term its
# `sub EXPORT` merged into `$*CU` (#15371, CompUnit::Util's `get-unit` /
# `get-lexical`). Checked against rakudo.

plan 4;

use lib $?FILE.IO.parent(2).sibling('lib').Str;
use VivifiedGlobalPackage;
use GeneratedLexicals;

BEGIN my $pkg = $*R.resolve-lexical-constant('VivifiedPkg', :current-scope-only).compile-time-value;
is $pkg.WHO<$answer>, 42, 'a package a module vivified under GLOBAL';

BEGIN my $unit = $*R.resolve-lexical-constant('unit-term', :current-scope-only)
    .compile-time-value;
is $unit, 'from-unit', 'a term a sub EXPORT merged into the unit';

{
    BEGIN my $inner = $*R.resolve-lexical-constant('unit-term').compile-time-value;
    is $inner, 'from-unit', 'seen from a nested block too';
}

BEGIN my $none = $*R.resolve-lexical-constant('no-such-term').defined;
nok $none, 'nothing for an undeclared name';

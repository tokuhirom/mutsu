use v6;
use Test;

plan 14;

# `==` / `!=` between two numeric allomorphs must compare their numeric bases,
# not the whole mixin (which also holds the string half). `<5.0> == <5>` was
# False because it fell to structural equality of the two mixins.
ok  <5.0> == <5>,  '<5.0> == <5> (RatStr vs IntStr)';
nok <5.0> != <5>,  '<5.0> != <5> is False';
ok  <5> == <5.0>,  '<5> == <5.0>';
ok  <1/2> == <0.5>, '<1/2> == <0.5>';
ok  <5.0> == 5,    '<5.0> == 5 (allomorph vs Int)';

# Smart-match against a numeric matcher compares by the allomorph's numeric base
# on either side.
ok  <5.0> ~~ <5>,  '<5.0> ~~ <5>';
ok  <5>   ~~ <5.0>,'<5> ~~ <5.0>';
ok  <5.0> ~~ 5,    '<5.0> ~~ 5';
ok  5.0   ~~ <5>,  '5.0 ~~ <5>';

# A Str LHS/matcher keeps string semantics — the allomorph's string half is used.
nok "5.0" ~~ <5>,  '"5.0" ~~ <5> is False (string mismatch)';
ok  <5.0> ~~ "5.0",'<5.0> ~~ "5.0" is True (string match)';
nok <5.0> ~~ "5",  '<5.0> ~~ "5" is False (string mismatch)';
ok  <5>   ~~ "5",  '<5> ~~ "5" is True';

# eqv still distinguishes the types (RatStr is not IntStr)
nok <5.0> eqv <5>, '<5.0> eqv <5> is False (different allomorph types)';

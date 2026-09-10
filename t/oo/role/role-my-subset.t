use v6;
use Test;

plan 4;

# A `my subset` inside a role is lexically scoped and private to the role body,
# which Raku allows (like `my class` / `my role`). Only an implicitly our-scoped
# `subset` is forbidden inside a role. (Regression: mutsu forbade all subsets.)

role R {
    my subset SmallInt of Int where * < 10;
    method small($x) { $x ~~ SmallInt }
}

class C does R {}

ok  C.new.small(5),  'my subset in a role matches an in-range value';
nok C.new.small(50), 'my subset in a role rejects an out-of-range value';

# `my subset` with an explicit predicate used in a method body also works.
role Named {
    my subset NonEmpty of Str where *.chars > 0;
    method ok-name($n) { $n ~~ NonEmpty }
}
class D does Named {}
ok  D.new.ok-name("x"), 'my subset (Str) accepts a non-empty string';
nok D.new.ok-name(""),  'my subset (Str) rejects an empty string';

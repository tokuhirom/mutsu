use Test;

# Multi methods rank their candidates with the same per-argument walk as multi
# subs (#12776): the nominal tier (typed positional count), the type distance,
# coercion parameters, `Nil`, core subsets and slurpies all rank alike, and
# rakudo's arity-mismatch rule applies to both. Expected values from rakudo.

plan 18;

class Coerce {
    multi method m(Int() $x) { "Int()" }
    multi method m(Cool $x)  { "Cool" }
}
is Coerce.m(42), 'Int()', 'a coercion param is as narrow as its target for an argument of it';
is Coerce.m("42"), 'Cool', 'and as wide as the type it accepts otherwise';

class NilArg {
    multi method m(Nil)        { "Nil" }
    multi method m(Str() $s)   { "Str()" }
    multi method a(Any $x)     { "Any" }
    multi method a(Mu $x)      { "Mu" }
}
is NilArg.m(Nil), 'Nil', 'a Nil candidate beats a coercion for a Nil argument';
is NilArg.a(Nil), 'Any', 'Nil ranks Any narrower than Mu';
multi sub nil-sub(Nil)      { "Nil" }
multi sub nil-sub(Str() $s) { "Str()" }
is nil-sub(Nil), 'Nil', 'a multi sub ranks Nil the same way';

class Sub {
    multi method m(UInt $x)    { "UInt" }
    multi method m(Numeric $x) { "Numeric" }
}
is Sub.m(3), 'UInt', 'UInt ranks as a refinement of Int';

class Typed {
    multi method m($x where * > 0, $y) { "where" }
    multi method m(Int $x, Int $y)     { "typed" }
}
is Typed.m(1, 2), 'typed', 'nominal types outrank a where refinement (typed tier)';

class SlurpyLast {
    multi method m(Int $x, *@r)    { "slurpy" }
    multi method m(Any $x, Any $y) { "two" }
}
class SlurpyFirst {
    multi method m(Any $x, Any $y) { "two" }
    multi method m(Int $x, *@r)    { "slurpy" }
}
is SlurpyLast.m(1, 2), 'two', 'arity mismatch: the non-slurpy candidate is narrower (declared last)';
is SlurpyFirst.m(1, 2), 'two', 'arity mismatch: the non-slurpy candidate is narrower (declared first)';
multi sub arity-sub(Int $x, *@r)    { "slurpy" }
multi sub arity-sub(Any $x, Any $y) { "two" }
is arity-sub(1, 2), 'two', 'a multi sub follows the same arity-mismatch rule';

class SameMinArity {
    multi method m(Int $x, *@r)     { "slurpy" }
    multi method m(Any $x, Any $y?) { "two-opt" }
}
is SameMinArity.m(1, 2), 'slurpy', 'equal minimum arity compares the shared position';

class OptTyped {
    multi method m(Int $x, Int $y?) { "opt" }
    multi method m(Int $x)          { "one" }
}
is OptTyped.m(1), 'one', 'an omitted typed optional does not count toward the typed tier';
multi sub opt-sub(Int $x, Int $y?) { "opt" }
multi sub opt-sub(Int $x)          { "one" }
is opt-sub(1), 'one', 'nor for a multi sub';

class Capture {
    multi method m(|)  { "cap" }
    multi method m($x) { "x" }
}
is Capture.m(1), 'x', 'a capture that swallows an argument is wider than a positional';

class Container {
    multi method m(Int $x) { "Int" }
    multi method m(Any $x) { "Any" }
}
my %h = a => 1;
is Container.m(%h<a>), 'Int', 'a hash element ranks by its contents';
my $v = 5;
is Container.m($v), 'Int', 'a variable ranks by its contents';

class Mixed {
    multi method m(Any:D $j, Mu $x) { "Any:D,Mu" }
    multi method m($j, @x)          { "pos" }
}
is Mixed.m(1, [1, 2]), 'pos', 'an implicit Positional out-narrows Mu on the second position';

class Seqy {
    multi method m(@raw)     { "list" }
    multi method m(Any:D $x) { "any" }
}
is Seqy.m("a b".words), 'list', 'a Seq ranks as the List it binds to @';

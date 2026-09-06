use v6;
use Test;

# ADR-0069: a definiteness-constrained type object (`Int:D`, `Any:U`) is an
# ordinary type object whose name carries the smiley. Every row below was
# measured against rakudo 2026-09-07; the rows that already agreed before the
# fix are pinned too, so a future refactor of the smiley model cannot silently
# regress them.

plan 74;

# --- .^name -----------------------------------------------------------------
is Int:D.^name,  'Int:D',  'Int:D.^name';
is Int:U.^name,  'Int:U',  'Int:U.^name';
is Str:D.^name,  'Str:D',  'Str:D.^name';
is Mu:D.^name,   'Mu:D',   'Mu:D.^name';
is Cool:D.^name, 'Cool:D', 'Cool:D.^name';

# `Any` and `Nil` are the two term keywords that also name real types; they
# used to be swallowed by the parser's keyword-literal table, so `Any:D` became
# plain `Any` and `Nil:D` became a Pair.
is Any:D.^name, 'Any:D', 'Any:D.^name keeps the smiley';
is Any:U.^name, 'Any:U', 'Any:U.^name keeps the smiley';
is Nil:D.^name, 'Nil:D', 'Nil:D.^name keeps the smiley';
is Nil:U.^name, 'Nil:U', 'Nil:U.^name keeps the smiley';

# `:_` asserts nothing and folds back to the unconstrained type.
is Any:_.^name, 'Any', 'Any:_ folds back to Any';
is Int:_.^name, 'Int', 'Int:_ folds back to Int';

# --- smart-matching ---------------------------------------------------------
nok (Any ~~ Any:D),     'a type object does not match Any:D';
ok  (Any ~~ Any:U),     'a type object matches Any:U';
ok  (Any ~~ Any:_),     'a type object matches Any:_';
ok  (Any.new ~~ Any:D), 'an instance matches Any:D';
nok (Any.new ~~ Any:U), 'an instance does not match Any:U';
ok  (42 ~~ Int:D),      '42 ~~ Int:D';
nok (42 ~~ Int:U),      '42 !~~ Int:U';
nok (Int ~~ Int:D),     'Int !~~ Int:D';
ok  (Int ~~ Int:U),     'Int ~~ Int:U';
ok  ("x" ~~ Str:D),     'Str instance ~~ Str:D';
nok (42 ~~ Str:D),      'wrong base type still fails';
nok (Str ~~ Int:U),     'wrong base type fails even for :U';
nok (Mu ~~ Any:U),      'Mu is not an Any, smiley or not';
ok  (1 ~~ Any:D),       '1 ~~ Any:D';

# --- .ACCEPTS ---------------------------------------------------------------
ok  Int:D.ACCEPTS(42),  'Int:D.ACCEPTS(42)';
nok Int:D.ACCEPTS(Int), 'Int:D.ACCEPTS(Int)';
nok Int:U.ACCEPTS(42),  'Int:U.ACCEPTS(42)';
ok  Int:U.ACCEPTS(Int), 'Int:U.ACCEPTS(Int)';

# --- .HOW is DefiniteHOW, not ClassHOW --------------------------------------
is Int:D.HOW.^name, 'Perl6::Metamodel::DefiniteHOW', 'Int:D.HOW is DefiniteHOW';
is Int:U.HOW.^name, 'Perl6::Metamodel::DefiniteHOW', 'Int:U.HOW is DefiniteHOW';
is Any:D.HOW.^name, 'Perl6::Metamodel::DefiniteHOW', 'Any:D.HOW is DefiniteHOW';
is Int.HOW.^name,   'Perl6::Metamodel::ClassHOW',    'unconstrained Int.HOW is ClassHOW';
is Int:_.HOW.^name, 'Perl6::Metamodel::ClassHOW',    'Int:_.HOW is ClassHOW';
is Int:D.HOW.name(Int:D), 'Int:D', 'DefiniteHOW.name reports the constrained name';

# --- .^base_type / .^definite ----------------------------------------------
is Int:D.^base_type.^name, 'Int', 'Int:D.^base_type recovers Int';
is Any:D.^base_type.^name, 'Any', 'Any:D.^base_type recovers Any';
is Str:U.^base_type.^name, 'Str', 'Str:U.^base_type recovers Str';
is Int:D.^definite, 1, 'Int:D.^definite is 1';
is Int:U.^definite, 0, 'Int:U.^definite is 0';
dies-ok { Int.^base_type },   'ClassHOW has no base_type';
dies-ok { Int.^definite },    'ClassHOW has no definite';
dies-ok { Int:_.^base_type }, ':_ folds to ClassHOW, which has no base_type';

# --- gist / raku / WHAT / defined -------------------------------------------
is Int:D.gist,       '(Int:D)', 'Int:D.gist';
is Int:D.raku,       'Int:D',   'Int:D.raku';
is Int:D.WHAT.^name, 'Int:D',   'Int:D.WHAT';
is Any:D.gist,       '(Any:D)', 'Any:D.gist';
is Any:D.raku,       'Any:D',   'Any:D.raku';
nok Int:D.defined,   'a constrained type object is still undefined';
nok Int:D.DEFINITE,  'Int:D.DEFINITE';

# --- identity ---------------------------------------------------------------
ok  (Int:D === Int:D), 'Int:D === Int:D';
nok (Int:D === Int),   'Int:D !=== Int';

# --- user-defined class, role, subset ---------------------------------------
class C {}
is C:D.^name, 'C:D', 'user class C:D.^name';
is C:U.^name, 'C:U', 'user class C:U.^name';
ok  (C.new ~~ C:D), 'C.new ~~ C:D';
nok (C ~~ C:D),     'C !~~ C:D';
ok  (C ~~ C:U),     'C ~~ C:U';
is C:D.^base_type.^name, 'C', 'C:D.^base_type';

role R {}
is R:D.^name, 'R:D', 'role R:D.^name';
is R:U.^name, 'R:U', 'role R:U.^name';

subset Sm of Int where * > 3;
is Sm:D.^name, 'Sm:D', 'subset Sm:D.^name';
ok (5 ~~ Sm:D), '5 ~~ Sm:D';

# --- the already-working constraint paths must not regress ------------------
sub f(Int:D $x) { "ok:$x" }
is f(42), 'ok:42', 'Int:D signature constraint accepts an instance';
dies-ok { f(Int) }, 'Int:D signature constraint rejects a type object';

sub g(Any:D $x) { 'ok' }
is g(1), 'ok', 'Any:D signature constraint accepts an instance';

{
    my Int:D $x = 5;
    is $x, 5, 'my Int:D $x = 5';
}
{
    my Any:D $z = 1;
    is $z, 1, 'my Any:D $z = 1';
}

multi m(Int:D $x) { 'D' }
multi m(Int:U $x) { 'U' }
is m(42),  'D', 'multi dispatches on :D';
is m(Int), 'U', 'multi dispatches on :U';

# --- stored in a variable, and in a list ------------------------------------
{
    my $t = Int:D;
    ok  (5 ~~ $t),   'a stored Int:D still matches an instance';
    nok (Int ~~ $t), 'a stored Int:D still rejects a type object';
    is  $t.^name, 'Int:D', 'a stored Int:D keeps its name';
}
is (Int:D, Int:U).map(*.^name).join(','), 'Int:D,Int:U', 'constrained types survive a list';

# --- a smiley is not an adverb ----------------------------------------------
# `:D` only ends a type name when it stands alone; `:Do` is still an adverb.
{
    my $p = (Any, :D);
    is $p.elems, 2, 'a separated :D is still a colonpair';
}

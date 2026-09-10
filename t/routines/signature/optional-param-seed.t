use v6;
use Test;

plan 16;

# An unpassed untyped optional parameter of a routine holds the Any type
# object (not Nil); a pointy block's holds Mu (PLAN 8.5, Nil-vs-Any).

sub opt-pos($p?) { $p }
is opt-pos().raku, 'Any', 'unpassed optional positional of a sub is Any';
is opt-pos().^name, 'Any', 'unpassed optional positional .^name is Any';
nok opt-pos().defined, 'unpassed optional positional is undefined';
is opt-pos(42), 42, 'passed optional positional binds the argument';

sub opt-named(:$k) { $k }
is opt-named().raku, 'Any', 'unpassed optional named of a sub is Any';
is opt-named(:k(7)), 7, 'passed optional named binds the argument';

sub opt-typed(Int $p?) { $p }
is opt-typed().raku, 'Int', 'unpassed typed optional seeds its type object';

sub opt-typed-named(Int :$k) { $k }
is opt-typed-named().raku, 'Int', 'unpassed typed optional named seeds its type object';

my &blk-pos = -> $p? { $p };
is blk-pos().raku, 'Mu', 'unpassed optional positional of a pointy block is Mu';

my &blk-named = -> :$k { $k };
is blk-named().raku, 'Mu', 'unpassed optional named of a pointy block is Mu';

my &blk-typed = -> Int $p? { $p };
is blk-typed().raku, 'Int', 'typed optional of a pointy block seeds its type object';

my class OptC {
    method m($p?) { $p }
    method mn(:$k) { $k }
}
is OptC.m().raku, 'Any', 'unpassed optional positional of a method is Any';
is OptC.mn().raku, 'Any', 'unpassed optional named of a method is Any';

sub opt-destructure($a, ($b, $c?)) { $c }
is opt-destructure(1, (2,)).raku, 'Any', 'unpassed optional in a sub-signature destructure is Any';

my @seen;
for 1, 2, 3 -> $a, $b? { @seen.push($b.raku) }
is @seen.join(';'), '2;Mu', 'short final chunk of a multi-param for loop seeds Mu';

my @typed-seen;
for 1, 2, 3 -> $a, Int $b? { @typed-seen.push($b.raku) }
is @typed-seen.join(';'), '2;Int', 'typed short final chunk seeds the type object';

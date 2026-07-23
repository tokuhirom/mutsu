use Test;

# PLAN 8.5 step 3: an uninitialized untyped `my $x` holds the Any type
# object (not Nil), matching Rakudo. The AST keeps the synthesized
# Literal(Nil) marker; the compiler substitutes the Any type object at
# RHS-emission time only (statement, expression, and block-final
# positions), so `$/`/`$!` and `:=` binds keep their Nil semantics.

plan 18;

my $x;
is $x.raku, 'Any', 'uninit my $x .raku is Any';
is $x.^name, 'Any', 'uninit my $x .^name is Any';
is $x.gist, '(Any)', 'uninit my $x gist is (Any)';
ok $x === Any, 'uninit my $x === Any';
nok $x.defined, 'uninit my $x is undefined';

is (my $y).raku, 'Any', 'expression-position (my $y) yields Any';
is (do { my $z }).raku, 'Any', 'do-block value of bare decl is Any';
sub f { my $v }
is f().raku, 'Any', 'block-final bare decl returns Any';

my ($a, $b);
is $a.raku, 'Any', 'comma-chained decl seeds Any';
nok $a =:= $b, 'two distinct uninit scalars are not =:=';

state $s;
is $s.raku, 'Any', 'uninit state scalar is Any';

my $bound := Nil;
ok $bound === Nil, ':= Nil bind keeps Nil';

my Int $t;
is $t.raku, 'Int', 'typed uninit scalar keeps its type object';

my &c;
is &c.raku, 'Callable', 'uninit &-sigil var keeps Callable';

my $d is default(42);
is $d, 42, 'is default(...) decl gets its default, not Any';

# Closure capture of an uninitialized lexical stays a live view: a later
# parent reassignment is observed (shared-cell semantics).
my $gate;
my $c2 = -> { $gate.defined ?? 'def' !! 'undef' };
$gate = 1;
is $c2(), 'def', 'closure over uninit lexical sees later assignment';

# Assigning Nil to an initialized var resets the container to Any again.
my $r = 5;
$r = Nil;
is $r.raku, 'Any', 'assigning Nil resets to Any';

# for-loop body redeclaration stays fresh each iteration.
my @seen;
for 1..2 { my $i; @seen.push($i.raku); $i = 9 }
is @seen.join(','), 'Any,Any', 'loop-body bare decl reseeds Any per iteration';

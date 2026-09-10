use v6;
use Test;

# Pin for the mixed positional+named light call path (NamedCallPlan with
# LightParamBind::Positional entries): a sub with both positional and named
# params used to take the full dispatch on every call; the light path must
# preserve its semantics — binding order, arity errors, type checks, junction
# autothreading, and parenthesized-Pair-as-positional.

plan 14;

sub f($x, :$c) { "$x/" ~ ($c // "-") }
is f(1), "1/-", 'positional only';
is f(1, :c(2)), "1/2", 'positional + named';
is f(:c(2), 1), "1/2", 'named before positional';

# Repeated calls through the light-call cache.
my @seen;
for ^3 { @seen.push(f($_, :c(9))) }
is-deeply @seen, ["0/9", "1/9", "2/9"], 'cached mixed calls bind fresh each time';

# Arity errors survive the cached fast path.
my $too-few;
{ f(); CATCH { default { $too-few = $_ } } }
ok $too-few.defined && $too-few.message.contains('Too few positionals'),
    'missing required positional is an arity error';
my $too-many;
{ f(1, 2); CATCH { default { $too-many = $_ } } }
ok $too-many.defined && $too-many.message.contains('Too many positionals'),
    'surplus positional is an arity error';

# A parenthesized Pair is a positional argument, not a named one.
sub pos-pair($x, :$a) { "x=" ~ ($x.^name eq 'Pair' ?? "pair" !! $x) ~ " a=" ~ ($a // "-") }
is pos-pair((:a(5))), "x=pair a=-", 'parenthesized Pair binds positionally';
is pos-pair(1, :a(5)), "x=1 a=5", 'named syntax binds the named param';

# Fast type constraint on the positional param.
sub typed(Int $n, :$m) { $n + ($m // 0) }
is typed(41, :m(1)), 42, 'typed positional binds';
my $type-err;
{ typed("s"); CATCH { default { $type-err = $_ } } }
ok $type-err.defined, 'type mismatch on positional raises';

# Junction in the positional slot still autothreads through the cached path.
is-deeply f(1|2, :c(0)).gist, any("1/0", "2/0").gist,
    'junction positional autothreads (cached light call)';

# Required named param on a mixed signature.
sub g($a, :$k!) { "$a $k" }
is g(1, :k(2)), "1 2", 'mixed with required named binds';
my $missing-named;
{ g(1); CATCH { default { $missing-named = $_ } } }
ok $missing-named ~~ X::AdHoc, 'missing required named on mixed throws X::AdHoc';

# Closure capture of both kinds of params.
sub cap($x, :$mult) { my $cl = { $x * ($mult // 1) }; $cl() }
is cap(21, :mult(2)), 42, 'closure captures positional and named params';

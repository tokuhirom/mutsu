use Test;

# #9454: `CallMethodDynamic`, `CallMethodDynamicMut` and
# `HyperMethodCallDynamic` used to carry their own copies of the method-call
# body, and the copies had drifted from the statically named opcodes (only one
# reified a Seq invocant, only the other FETCHed a Proxy argument, a run-time
# `"WHAT"` never reached a user method). They now resolve the run-time name
# and delegate to the static body, so every scenario here must give the same
# answer through `$obj.meth`, `$obj."meth"` and `$obj."$name"`.

plan 20;

class A { method WHAT { "mine" }; method foo($x = 1) { "Af$x" } }
class B is A { method foo($x = 1) { "Bf$x" } }

my $what = "WHAT";
is A.new."$what"(), A.new."WHAT"(), 'a run-time "WHAT" calls the user method, like a quoted one';
is A.new."$what"(), 'mine', 'which is the user method';
ok 42."$what"() =:= Int, 'and without a user method it is the built-in';
ok 42."WHAT"() =:= Int, 'as is a quoted "WHAT" without a user method';

my $foo = "foo";
is-deeply B.new.+"$foo"(3), B.new.+foo(3), '.+ with a run-time name matches the static form';
is-deeply B.new.*"$foo"(), B.new.*foo(), '.* with a run-time name matches the static form';
is B.new.?"nope"(), B.new.?nope(), '.? on a missing method matches the static form';

# A Seq invocant is reified for a non-lazy method on every form.
my $elems = "elems";
my $s1 = (1..3).map(* * 2);
my $s2 = (1..3).map(* * 2);
is $s1."$elems"(), $s2.elems, 'a Seq invocant through a run-time name';

# A Proxy argument to a mutator is FETCHed on every form.
my $px = Proxy.new(FETCH => { 7 }, STORE => -> $, $ { });
my $push = "push";
my (@p, @q);
@p.push($px);
@q."$push"($px);
is-deeply @q, @p, 'a Proxy argument to a mutator through a run-time name';

# A mutating method on a named receiver (`CallMethodDynamicMut`).
my ($u1, $u2) = "abc", "abc";
my $uc = "uc";
$u1 .= uc;
$u2 .= "$uc"();
is $u2, $u1, '.= with a run-time name matches the static form';

# A Callable in the name position is invoked with the receiver.
my &c = -> $s, $n { "code:$s:$n" };
my $code = &c;
is "x".$code(1), 'code:x:1', 'a Callable name gets the receiver as its first argument';
is-deeply "x".+$code(1), ('code:x:1',), 'and .+ wraps its single result';

# Hyper forms.
class C { method m { "m" } }
my @cs = C.new xx 2;
my $m = "m";
is-deeply @cs>>."$m"(), @cs>>.m, 'hyper with a run-time name matches the static form';
my @n = 1, (2, 3);
is-deeply @n>>."$elems"(), @n>>.elems, 'a nodal run-time name stays at the node level';
sub dbl($x) { $x * 2 }
is-deeply @n>>.&dbl, [2, (4, 6)], 'a hyper Callable still descends';
my %h = a => 1, b => 2;
my $succ = "succ";
is-deeply %h>>."$succ"(), %h>>.succ, 'a Hash hyper keeps its keys through a run-time name';
is-deeply set(1, 2)>>."$succ"(), set(1, 2)>>.succ, 'a QuantHash hyper through a run-time name';

# A pseudo-method name spelled as a string, per element.
is-deeply (A.new, A.new)>>."$what"(), ('mine', 'mine'), 'hyper run-time "WHAT" calls the user method';
is-deeply (A.new, A.new)>>."WHAT"(), ('mine', 'mine'), 'as does a quoted hyper "WHAT"';
is-deeply (1, 2)>>."$what"(), (Int, Int), 'and without a user method it is the built-in per element';

use Test;

# A routine with a defaulted or `?`-optional trailing positional now reaches the
# cached positional light-call path: the omitted parameters are filled from a
# constant table computed once at registration, instead of the routine falling
# through to a full by-name resolve on every call.
#
# The point of the test is that admitting those signatures did not change what
# they BIND. Every case below is pinned against rakudo's answer.

plan 29;

# --- a literal default -------------------------------------------------------
sub lit($x, $y = 5) { "$x/$y" }
is lit(1),    "1/5", 'literal default fills when omitted';
is lit(1, 9), "1/9", 'literal default is overridden when supplied';

sub lit-str($x, $y = 'z') { "$x/$y" }
is lit-str(1),      "1/z", 'Str literal default fills';
is lit-str(1, 'q'), "1/q", 'Str literal default is overridden';

sub lit-bool($x, $y = True) { "$x/$y" }
is lit-bool(1),        "1/True",  'Bool literal default fills';
is lit-bool(1, False), "1/False", 'Bool literal default is overridden';

sub lit-two($x, $y = 1, $z = 2) { "$x/$y/$z" }
is lit-two(1),       "1/1/2", 'two defaults both fill';
is lit-two(1, 8),    "1/8/2", 'a partially-supplied tail fills only the rest';
is lit-two(1, 8, 7), "1/8/7", 'a fully-supplied call fills nothing';

# --- a bare `?` binds the parameter's type object -----------------------------
sub opt($x, $y?) { "$x/{$y.^name}/{$y.defined}" }
is opt(1),    "1/Any/False", 'an untyped `?` binds Any, undefined';
is opt(1, 9), "1/Int/True",  'a supplied `?` binds the argument';

sub opt-typed($x, Int $y?) { "$x/{$y.^name}/{$y.defined}" }
is opt-typed(1),    "1/Int/False", 'a typed `?` binds its own type object';
is opt-typed(1, 9), "1/Int/True",  'a supplied typed `?` binds the argument';

# --- a constrained default is checked against its own constraint --------------
sub typed($x, Int $y = 7) { "$x/$y" }
is typed(1),    "1/7", 'Int default fills';
is typed(1, 9), "1/9", 'Int default is overridden';
# (through a variable, so rakudo cannot reject the call at compile time and the
# check really runs in the binder)
my $not-an-int = "no";
dies-ok { typed(1, $not-an-int) }, 'a supplied argument is still type-checked';

# --- shapes that must STAY on the general binder ------------------------------
# A non-constant default is an arbitrary expression evaluated per call, with the
# parameter shadowed by its own type object. It may read earlier parameters.
sub nonconst($x, $y = $x + 1) { "$x/$y" }
is nonconst(1),    "1/2", 'a non-constant default still evaluates per call';
is nonconst(1, 9), "1/9", 'a non-constant default is overridden';
is nonconst(4),    "4/5", 'a non-constant default reading an earlier param';

# A container default must be a FRESH container on every call -- a shared
# constant would accumulate across calls.
sub cont($x, $y = [1, 2]) { $y.push(3); $y.elems }
is cont(1), 3, 'a container default is fresh (call 1)';
is cont(1), 3, 'a container default is fresh (call 2)';
is cont(1), 3, 'a container default is fresh (call 3)';

# A `where` constraint on a defaulted parameter is still enforced.
sub whered($x, $y where * > 0 = 4) { "$x/$y" }
is whered(1),    "1/4", 'a `where` parameter with a default fills';
is whered(1, 9), "1/9", 'a `where` parameter with a default is overridden';

# --- arity errors keep their shape -------------------------------------------
# Spelled as EVAL'd source: rakudo rejects each of these at COMPILE time, so a
# block form would abort the whole file there rather than throwing at runtime.
eval-dies-ok 'sub ar($x, $y = 5) { }; ar()',
    'a call short of the mandatory prefix is still an arity error';
eval-dies-ok 'sub ar($x, $y = 5) { }; ar(1, 2, 3)',
    'a surplus argument is still an arity error';
eval-dies-ok 'sub ar2($x, $y = 1, $z = 2) { }; ar2(1, 2, 3, 4)',
    'a surplus argument past a filled tail is still an arity error';

# The parser gives every parenthesized zero-argument call a synthetic
# `__mutsu_test_callsite_line` Pair as its sole argument. The positional light
# binder used to count it: with the cache warm, `f(); ` bound the marker itself
# and the routine saw a `Pair`. Admitting optionals here makes the same leak
# reachable for a merely-omitted parameter, so it is stripped now.
sub warmed($a) { $a }
warmed(1);
eval-dies-ok 'sub warmed2($a) { $a }; warmed2(1); warmed2()',
    'a zero-arg call to a warmed 1-param sub is an arity error, not a Pair bind';
sub warm-opt($a?) { $a }
warm-opt(1);
is warm-opt().^name, 'Any',
    'a zero-arg call to a warmed optional-param sub fills, not binds the marker';

use Test;

# `R(42)` in `but`/`does` position INITIALISES the single public attribute of an
# unparameterised role; it is not a role parameterisation. mutsu recorded the
# argument as a type argument either way, so:
#
#     role R { has $.x }
#     say (1 but R(2)).^name;   # raku: Int+{R}   mutsu: Int+{R[Int]}
#
# and, because the type arguments also feed the composed `.WHAT` identity, two
# values mixed with the same role but different initialisers had different
# types. The mirror-image error was that a genuine parameterisation was ALSO
# treated as an initialiser, so `role Q[::T] { has $.y }` had `Q[Str]` assign
# `Str` to `$.y`.
#
# Every expectation below was measured against raku v2026.07 first.

plan 26;

role R  { has $.x }
role P[::T] { }
role Q[::T] { has $.y }
role D[::T = Int] { }
role N  { }
role T2 { has $.a; has $.b }

# --- an initialiser does not appear in the name ---------------------------

is (1 but R(2)).^name, 'Int+{R}', 'an attribute initialiser is not a type parameter';
is (1 does R(2)).^name, 'Int+{R}', '... for `does` as well as `but`';
is ('Life' but R(42)).^name, 'Str+{R}', '... on a Str base type';
is (1 but R).^name, 'Int+{R}', 'the bare composition is unchanged';

# ... and the attribute is still initialised.
is (1 but R(2)).x, 2, 'the attribute is initialised';
is (1 does R(2)).x, 2, '... through `does` too';
is ('Life' but R(42)).x, 42, '... on a Str base type';

# --- an uninitialised scalar attribute is Any, not Nil --------------------

is-deeply (1 but R).x, Any, 'an uninitialised scalar attribute is Any';
nok (1 but R).x.defined, '... and is undefined';
{
    role Narrowed { has Int $.n }
    is-deeply (1 but Narrowed).n, Int, 'a typed one narrows to its declared type';
    role Aggr { has @.a; has %.h }
    is-deeply (1 but Aggr).a, [], 'an @ attribute defaults to []';
    is-deeply (1 but Aggr).h, {}, 'a % attribute defaults to {}';
}

# --- a genuine parameterisation still keeps its arguments in the name -----

is (1 but P[Int]).^name, 'Int+{P[Int]}', 'a parameterised role keeps its type argument';
is (1 but D).^name, 'Int+{D}', 'a defaulted parameter is not spelled out';
is (1 but D[Str]).^name, 'Int+{D[Str]}', '... but an explicit one is';
ok (1 but P[Int]).does(P), '.does still sees a parameterised role';
ok (1 but R(2)).does(R), '.does still sees an attribute-initialised role';

# --- a parameterisation is NOT an attribute initialiser ------------------

is (1 but Q[Str]).^name, 'Int+{Q[Str]}', 'a role that is parameterised AND carries an attribute';
is-deeply (1 but Q[Str]).y, Any, '... leaves the attribute at its default';

# --- an attribute default may reference the role's own type parameter -----
#
# Before the split above, `R[42]` reached `$.a` through the INITIALISER path by
# accident, so nothing ever had to bind `$v`; the defaulted spelling had no path
# at all. Both go through the parameter binding now.

{
    role Val[$v] { has $.a = $v }
    is (1 but Val[42]).a, 42, 'an attribute default reads a value parameter';
    is (1 but Val[42]).^name, 'Int+{Val[Int]}', '... and the name shows the argument type';

    role Defaulted[$v = 7] { has $.a = $v }
    is (1 but Defaulted).a, 7, 'a DEFAULTED value parameter reaches the attribute';
    is (1 but Defaulted).^name, 'Int+{Defaulted}', '... without appearing in the name';

    role Typed[::T] { has $.a = T }
    is-deeply (1 but Typed[Str]).a, Str, 'an attribute default reads a type parameter';

    role TypedDefault[::T = Int] { has $.a = T }
    is-deeply (1 but TypedDefault).a, Int, '... including its declared default';
}

# --- the initialisation error is unchanged -------------------------------

is (try { 1 but N(5) } // $!.^name), 'X::Role::Initialization',
   'a role with no public attribute refuses an initialiser';

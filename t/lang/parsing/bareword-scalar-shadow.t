use Test;

# `$bar` and the term `bar` are different symbols: `my $bar` declares no
# bare word (#11898). mutsu keeps a scalar sigil-stripped under the same `env`
# key a sigilless binding (`my \bar`, `-> \bar`) uses, so the compiler tells the
# bare-word read that the same-named local is a `$`-scalar, and the read must
# neither answer that scalar nor, with nothing else to claim it, a string.

plan 30;

# -- the bare word is not the scalar ---------------------------------------

throws-like { EVAL 'my $bar = 3; bar' }, X::Undeclared::Symbols,
    message => /'Undeclared routine'/,
    'a bare word does not read a same-named $-scalar';

throws-like { EVAL 'my $bar = Int; bar' }, X::Undeclared::Symbols,
    'nor a type object it holds';

throws-like { EVAL 'my $bar; bar' }, X::Undeclared::Symbols,
    'nor an unassigned one';

{
    my $proc = run $*EXECUTABLE, '-e', 'my $bar = 3; say bar', :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $out, '', 'a program reading the bare word prints nothing';
    ok $err.contains('Undeclared routine'), 'and dies as an undeclared routine';
    isnt $proc.exitcode, 0, 'with a failing exit status';
}

is EVAL('my $bar = 3; $bar'), 3, 'the scalar itself is untouched';

# The scalar belongs to an enclosing frame.
throws-like { EVAL 'my $bar = 3; sub f { bar }; f()' }, X::Undeclared::Symbols,
    'a bare word in a sub does not read the enclosing scope\'s $-scalar';

throws-like { EVAL 'my $bar = 3; my &c = { bar }; c()' }, X::Undeclared::Symbols,
    'nor in a closure';

# -- anything else of that name still wins -----------------------------------

my $baz = 3;
sub baz { 7 }
is baz, 7, 'a sub of the name is the bare word';
is $baz, 3, 'beside the scalar';

{
    sub nested-baz { baz }
    is nested-baz, 7, 'a sub of the name is the bare word from a nested sub too';
}

my $Bar = 5;
class Bar { }
is Bar.^name, 'Bar', 'a class of the name is the bare word';
is $Bar, 5, 'beside the scalar';

{
    # A lexical class is bound in `env` under the very key its same-named
    # scalar uses: it must stay reachable after the scalar is assigned.
    my class lexfoo { has $.x = 1 }
    my $lexfoo = lexfoo.new;
    my $other = lexfoo.new(x => 2);
    is $other.x, 2, 'a lexical class of the name is the bare word beside the scalar';
}

my $green = 5;
enum Col <red green>;
is green.value, 1, 'an enum key of the name is the bare word';
is $green, 5, 'beside the scalar';

my $q = 1;
{
    my \q = 2;
    is q, 2, 'a sigilless binding of the name is the bare word';
}
is $q, 1, 'beside the scalar';

# -- the core terms and builtins a scalar may be named after -----------------

{
    my $pi = 3;
    my $e = 1;
    my $Inf = 1;
    my $time = 4;
    is pi.round(0.01), 3.14, '`pi` beside $pi';
    is e.round(0.01), 2.72, '`e` beside $e';
    is Inf, Inf, '`Inf` beside $Inf';
    ok time > 0, '`time` beside $time';
}

# -- sigilless parameters stay terms -----------------------------------------

{
    my $x = 100;
    sub plus-one(\x) { x + 1 }
    is plus-one(4), 5, 'a sigilless sub parameter, with a same-named scalar outside';
}

{
    my @seen;
    given 7 -> \ex { @seen.push(ex) }
    with 3 -> \w { @seen.push(w) }
    if 4 -> \i4 { @seen.push(i4) }
    is @seen, [7, 3, 4], 'a sigilless given/with/if parameter';
}

{
    my @q = 3, 4, 5;
    my @seen;
    while @q.shift -> \v { @seen.push(v) }
    is @seen, [3, 4, 5], 'a sigilless while parameter';
}

{
    my $n = 0;
    my @seen;
    until ($n += 1) > 3 -> \c { @seen.push(c) }
    is @seen, [False, False, False], 'a sigilless until parameter';
}

{
    my @l = 1, 2, 3;
    my @subs;
    while @l.shift -> \z { @subs.push({ z * 10 }) }
    is @subs.map({ $_() }), (10, 20, 30), 'each while iteration binds a fresh term';
}

{
    my @l = 1, 2;
    my $caught = False;
    while @l.shift -> \z {
        try { z = 9; CATCH { default { $caught = True } } }
    }
    ok $caught, 'a sigilless while parameter is read-only';
}

# -- a declaration elsewhere does not leak ------------------------------------

{
    { my $zork = 1 }
    sub zork { 'sub' }
    is zork, 'sub', 'a scalar of an exited scope does not hide a sub';
}


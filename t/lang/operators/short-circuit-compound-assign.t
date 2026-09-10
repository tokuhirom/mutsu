use Test;

# `//=`, `||=` and `&&=` are defined as `$x // ($x = v)` etc.: when the short
# circuit KEEPS the current value, NO assignment happens at all. mutsu used to
# desugar them to the flat `$x = ($x // v)`, which stored unconditionally --
# observable whenever the left-hand side is not assignable, and whenever a
# `Proxy` would see a spurious STORE.

# --- the short circuit stores nothing -------------------------------------

{
    my $x := 42;
    lives-ok { $x //= 5 }, '//= on a := -bound literal does not store when defined';
    is $x, 42, '... and leaves the bound value alone';
}

{
    my $x := 42;
    lives-ok { $x ||= 5 }, '||= on a := -bound literal does not store when true';
    is $x, 42, '... and leaves the bound value alone';
}

{
    sub f($x) { $x //= 5; $x }
    is f(3), 3, '//= on a defined readonly parameter is a no-op, not an error';
}

throws-like { sub g($x) { $x //= 5 }; g(Int) }, X::AdHoc,
    '//= on an UNdefined readonly parameter still throws';

{
    my $stores = 0;
    my $v = 1;
    my $p := Proxy.new(FETCH => method () { $v }, STORE => method ($n) { $stores++; $v = $n });
    $p //= 9;
    is $stores, 0, '//= does not call a Proxy STORE when the value is defined';
    is $v, 1, '... and the underlying value is untouched';
}

# --- the non-short-circuit path still stores ------------------------------

{
    my $a;
    $a //= 7;
    is $a, 7, '//= assigns when undefined';
    my $b = 0;
    $b ||= 5;
    is $b, 5, '||= assigns when false';
    my $c = 5;
    $c &&= 9;
    is $c, 9, '&&= assigns when true';
    my $d = 0;
    $d &&= 9;
    is $d, 0, '&&= keeps a false value';
}

# --- what the expression returns ------------------------------------------
# It is the LHS *container* when there is one, and the bare value when there
# is not -- so an outer metaop writes through in the first case and dies in
# the second (roast S03-metaops/misc.t pins both).

{
    my $a;
    ($a //= 42) += 10;
    is $a, 52, 'the storing branch returns the container (52)';
    ($a //= 42) += 10;
    is $a, 62, 'the short-circuiting branch returns the container too (62)';
}

throws-like { my $a := 42; ($a //= 42) += 10 }, X::Assignment::RO,
    'a containerless LHS returns a value, so the outer metaop throws RO';

done-testing;

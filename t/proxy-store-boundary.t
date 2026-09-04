use Test;

# ADR-0040's store boundary, Proxy half.
#
# Raku reads the RHS of `=` in value context, so a `Proxy` that lands *inside* a
# container -- a `$` Scalar, an Array element, a Hash value, an attribute -- is
# FETCHed on the way in and the element that lands is a plain value. mutsu used
# to store the Proxy itself and re-FETCH it on every read, which is observable
# the moment the Proxy's backing lexical changes afterwards, and which made an
# `is rw` alias to such an element fire the Proxy's STORE instead of writing the
# array (ADR-0040 §9, news/2026-09/proxy-fetches-at-the-container-store.md).
#
# Every block below is self-contained: `$n` is the backing lexical, so `5` means
# the store snapshotted (raku) and `9` would mean the Proxy survived the store.

plan 28;

# --- scalar containers -------------------------------------------------------

{
    my $n = 5;
    my $s = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is $s, 5, 'a Proxy assigned to a `my $` scalar FETCHes at the store';
    is $s.VAR.^name, 'Scalar', '... and the container is a plain Scalar';
}

{
    my $n = 5;
    my $s;
    $s = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is $s, 5, 'a re-assignment to an existing scalar FETCHes too';
}

{
    my $n = 5;
    sub state-holder() { state $s = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }); $s }
    state-holder();
    $n = 9;
    is state-holder(), 5, 'a `state $s = Proxy` initializer FETCHes';
}

# A `:=` bind is the exception: it installs the Proxy itself.
{
    my $n = 5;
    my $b := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    is $b.VAR.^name, 'Proxy', 'a `:=` bind keeps the Proxy container';
    $n = 9;
    is $b, 9, '... so it still FETCHes live';
    $b = 42;
    is $n, 42, '... and assigning to it fires STORE';
}

# Reading a bound Proxy in an rvalue is itself a value-context read.
{
    my $n = 5;
    my $b := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    my $copy = $b;
    $n = 9;
    is $copy, 5, 'copying a Proxy-bound variable snapshots the FETCHed value';
}

# --- Array elements ----------------------------------------------------------

{
    my $n = 5;
    my @a = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    is @a.raku, '[5]', 'a Proxy assigned into an Array is FETCHed, not stored';
    $n = 9;
    is @a[0], 5, '... and the element does not track the Proxy afterwards';
}

{
    my $n = 5;
    my @a = (1, Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }), 3);
    $n = 9;
    is-deeply @a, [1, 5, 3], 'each element of a list assignment FETCHes';
}

{
    my $n = 5;
    my @a = [1, Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v })];
    $n = 9;
    is-deeply @a, [1, 5], 'a `[...]` literal FETCHes its elements at construction';
}

{
    my $n = 5;
    my @a;
    @a[0] = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is @a[0], 5, 'an indexed element assignment FETCHes';
}

{
    my $n = 5;
    my @a;
    @a[0][1] = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is @a[0][1], 5, 'a chained subscript assignment FETCHes';
}

{
    my $n = 5;
    my @a;
    @a[0;1] = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is @a[0;1], 5, 'a multi-dim assignment FETCHes';
}

for <push unshift append prepend> -> $mutator {
    my $n = 5;
    my @a;
    @a."$mutator"(Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }));
    $n = 9;
    is @a[0], 5, "a $mutator of a Proxy stores the FETCHed value";
}

{
    my $n = 5;
    my @a;
    @a.splice(0, 0, Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }));
    $n = 9;
    is @a[0], 5, '`@a.splice` stores the FETCHed value';
}

# --- Hash values -------------------------------------------------------------

{
    my $n = 5;
    my %h;
    %h<k> = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is %h<k>, 5, 'a hash element assignment FETCHes';
}

{
    my $n = 5;
    my %h = (k => Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }));
    $n = 9;
    is %h<k>, 5, 'a whole-hash assignment FETCHes its values';
}

{
    my $n = 5;
    my $h = { k => Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v }) };
    $n = 9;
    is $h<k>, 5, 'a hash literal FETCHes its values at construction';
}

{
    my $n = 5;
    my %h;
    %h.push((k => Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v })));
    $n = 9;
    is %h<k>, 5, '`%h.push(k => $proxy)` stores the FETCHed value';
}

{
    my $n = 5;
    my %h;
    %h{1;2} = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is %h{1;2}, 5, 'a multi-dim hash assignment FETCHes';
}

# --- attributes --------------------------------------------------------------

{
    my $n = 5;
    class C { has $.v is rw }
    my $c = C.new;
    $c.v = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is $c.v, 5, 'an rw-accessor assignment FETCHes';
}

# --- the consequence the boundary exists for ---------------------------------

# `substr-rw` returns a Proxy. Assigning it into a plain scalar snapshots the
# substring; only the direct lvalue form writes back through STORE.
{
    my $str = "hello";
    my $s = substr-rw($str, 0, 2);
    $s = "XY";
    is $str, 'hello', 'a substr-rw result assigned into a scalar no longer writes back';
    my $str2 = "hello";
    substr-rw($str2, 0, 2) = "XY";
    is $str2, 'XYllo', '... while the direct lvalue form still does';
}

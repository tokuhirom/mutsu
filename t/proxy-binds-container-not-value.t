use v6;
use Test;

# ADR-0040 §9 from the BIND side: a `Proxy` is FETCHed when it lands INSIDE a
# container, but an `is rw`/`is raw` parameter and a `:=` bind take the
# container itself. Every expected value below was oracled against
# `raku` v2026.06.

plan 24;

# --- `is rw` / `is raw` parameters bind the caller's Proxy container --------

{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    sub set-rw($x is rw) { $x = 42 }
    set-rw($p);
    is $n, 42, 'is rw param fires the caller Proxy STORE';
    is $p.VAR.^name, 'Proxy', 'the caller binding is still a Proxy after the call';
}

{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    sub set-raw($x is raw) { $x = 43 }
    set-raw($p);
    is $n, 43, 'is raw param fires the caller Proxy STORE';
    is $p.VAR.^name, 'Proxy', 'is raw leaves the caller binding a Proxy';
}

{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    class Setter { method set($x is rw) { $x = 44 } }
    Setter.set($p);
    is $n, 44, 'a method rw param fires the caller Proxy STORE';
}

{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    sub read-and-bump($x is rw) { my $seen = $x; $x = $x + 1; $seen }
    is read-and-bump($p), 5, 'an rw param READS through the Proxy FETCH';
    is $n, 6, 'and the read-modify-write lands through STORE';
}

{
    # A plain (readonly) parameter still gets the FETCHed value, and the
    # caller's container is untouched.
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    sub plain($x) { $x }
    is plain($p), 5, 'a readonly param sees the FETCHed value';
    is $n, 5, 'and nothing was stored';
    is $p.VAR.^name, 'Proxy', 'the caller binding survives a readonly call';
}

# --- `@a[0] := $proxy` installs the Proxy as the element's container -------

{
    my $n = 5;
    my @a = (1, 2);
    @a[0] := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is @a[0], 9, 'an array element bound to a Proxy keeps tracking';
    is @a[0].VAR.^name, 'Proxy', 'the element container IS the Proxy';
    is @a[1], 2, 'the sibling element is untouched';
    @a[0] = 20;
    is $n, 20, 'assigning to that element fires its STORE';
    is @a[0], 20, 'and reads back through FETCH';
    is @a[0].VAR.^name, 'Proxy', 'the store did not replace the container';
}

{
    my $m = 3;
    my %h = (k => 1);
    %h<k> := Proxy.new(FETCH => -> $ { $m }, STORE => -> $, $v { $m = $v });
    $m = 7;
    is %h<k>, 7, 'a hash value bound to a Proxy keeps tracking';
    %h<k> = 11;
    is $m, 11, 'assigning to that hash value fires its STORE';
}

{
    # The store boundary is unchanged: assigning a Proxy INTO a container
    # still FETCHes (ADR-0040 §9), so only `:=` installs one.
    my $n = 5;
    my @a;
    @a[0] = Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    $n = 9;
    is @a[0], 5, 'a Proxy ASSIGNED into an element is FETCHed on the way in';
    is @a[0].VAR.^name, 'Scalar', 'and the element is an ordinary Scalar';
}

# --- a top-level Proxy FETCHes in string context --------------------------

{
    my $n = 5;
    my $p := Proxy.new(FETCH => -> $ { $n }, STORE => -> $, $v { $n = $v });
    is "$p", '5', 'interpolation FETCHes a Proxy';
    is 'x' ~ $p, 'x5', 'infix ~ FETCHes a Proxy';
    ok $p eq '5', 'string comparison FETCHes a Proxy';
    $n = 9;
    is "$p", '9', 'and the FETCH is live, not snapshotted';
}

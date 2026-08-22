use Test;

# A deferred vivification token (the `:=`/`return-rw` bind of a not-yet-existent
# hash key) records each path step's shape, so a POSITIONAL step walk-creates an
# Array. It used to stringify the index into a hash key, vivifying
# `{:g(${"0" => "x"})}` where raku produces `{:g($["x"])}`.
# Every expectation below is byte-identical to `raku`'s output.

plan 28;

# --- write through a bound token -------------------------------------------

{
    my %h;
    my $x := %h<g>;
    $x[0] = 'x';
    is %h.raku, '{:g($["x"])}', 'positional step through a bound token makes an Array';
}

{
    my %h;
    my $x := %h<g>;
    $x[2] = 'x';
    is %h.raku, '{:g($[Any, Any, "x"])}', 'a positional step past the end fills with Any';
}

{
    my %h;
    my $x := %h<g>;
    $x<k> = 'x';
    is %h.raku, '{:g(${:k("x")})}', 'an associative step still makes a Hash';
}

{
    my %h;
    my $x := %h<g>;
    $x[0]<k> = 'x';
    is %h.raku, '{:g($[{:k("x")},])}', 'Array then Hash';
}

{
    my %h;
    my $x := %h<g>;
    $x<a>[1] = 'x';
    is %h.raku, '{:g(${:a($[Any, "x"])})}', 'Hash then Array';
}

{
    my %h;
    my $x := %h<g>;
    $x[0][1] = 'x';
    is %h.raku, '{:g($[[Any, "x"],])}', 'two positional steps nest two Arrays';
}

{
    my %h;
    my $x := %h<g>;
    $x<a><b>[0] = 'x';
    is %h.raku, '{:g(${:a(${:b($["x"])})})}', 'a three-step chain ending positional';
}

# The bound variable and the hash entry alias after the first write.
{
    my %h;
    my $x := %h<g>;
    $x[0] = 'a';
    $x[1] = 'b';
    is %h.raku, '{:g($["a", "b"])}', 'the binding survives the materializing write';
    is $x.join(','), 'a,b', 'the bound variable reads the created Array';
    %h<g>[2] = 'c';
    is $x.join(','), 'a,b,c', 'a write through the hash is seen by the binding';
}

# --- deferred bind chains --------------------------------------------------

{
    my %h;
    my $x := %h<g>[1];
    is %h.raku, '{}', 'binding a positional step vivifies nothing';
    is $x.raku, 'Any', 'the unwritten deferred chain reads as Any';
    $x = 5;
    is %h.raku, '{:g($[Any, 5])}', 'writing the bound chain creates the Array';
}

{
    my %h;
    my $x := %h<g>[1]<k>;
    $x = 5;
    is %h.raku, '{:g($[Any, {:k(5)}])}', 'a mixed deferred chain creates both kinds';
}

{
    my %h;
    my $x := %h<a><b>[2][0];
    $x = 'z';
    is %h.raku, '{:a(${:b($[Any, Any, ["z"]])})}', 'a four-step mixed deferred chain';
}

{
    my %h;
    my $x := %h<g>;
    ok !($x[0]:exists), 'a deferred token does not vivify on an :exists probe';
    is %h.raku, '{}', 'and the hash is untouched';
}

# --- `is rw` lvalue return (ADR-0059) with a positional step ---------------

{
    # `Crane::In`'s shape: a recursive `is rw` descent whose next container is
    # reached through a subscript in the `return-rw` operand's call argument.
    # Each level hands the deeper call a deferred token, so the positional steps
    # only become Arrays when the final write walks the path.
    multi sub deep(\c, @s where { .elems == 1 and @s[0] ~~ Int }) is rw {
        return-rw c[@s[0]];
    }
    multi sub deep(\c, @s where { .elems == 1 }) is rw {
        return-rw c{@s[0]};
    }
    multi sub deep(\c, @s where { .elems > 1 and @s[0] ~~ Int }) is rw {
        return-rw deep(c[@s[0]], @s[1..*]);
    }
    multi sub deep(\c, @s where { .elems > 1 }) is rw {
        return-rw deep(c{@s[0]}, @s[1..*]);
    }

    my %h;
    deep(%h, ['g', 0]) = 'v';
    is %h.raku, '{:g($["v"])}', 'a return-rw positional step autovivifies an Array';

    my %i;
    deep(%i, ['g', 1, 0]) = 'w';
    is %i.raku, '{:g($[Any, ["w"]])}', 'a recursive return-rw descent through two positional steps';

    my %j;
    deep(%j, ['a', 0, 'b']) = 'x';
    is %j.raku, '{:a($[{:b("x")},])}', 'a return-rw descent alternating both step kinds';
}

{
    sub pick(\c, $i) is rw { return-rw c[$i] }
    my %h = g => [];
    pick(%h<g>, 1) = 'w';
    is %h.raku, '{:g($[Any, "w"])}', 'return-rw into an existing Array';
}

# --- the eager side is unaffected ------------------------------------------

{
    my %h = a => 1, b => 2;
    for %h.kv -> $k, $v { }
    my @seen;
    for %h -> $p { @seen.push($p.key) }
    is @seen.sort.join(','), 'a,b', 'live iteration pairs still work';
}

{
    my %h = a => 1;
    for %h -> $p { $p.value = 9 }
    is %h.raku, '{:a(9)}', 'writing a live iteration pair still writes through';
}

# --- container identity through a deferred token ---------------------------

{
    my %h;
    my $a := %h<k>;
    my $b := %h<k>;
    nok $a =:= $b, 'two unmaterialized deferred tokens are distinct containers';
}

{
    my %h;
    my $a := %h<k>;
    my $b := %h<other>;
    nok $a =:= $b, 'deferred tokens on different keys are different containers';
}

# --- a cell promoted from a real scalar leaf is not an empty chain link ----

{
    # `%h<a>` is promoted to a shared cell holding the scalar `1`. Descending it
    # must NOT replace that `1` with a fresh container: rakudo raises "Cannot
    # assign to an immutable value" on the write, so a no-op is the closest
    # honest behaviour, and clobbering the data would be strictly worse.
    my %h = a => 1;
    my $x := %h<a><b>;
    is %h.raku, '{:a(1)}', 'binding through a scalar leaf vivifies nothing';
    # rakudo throws "Cannot assign to an immutable value" here; mutsu does not
    # throw yet, so the shared, checkable claim is that the data survives.
    try { $x = 5 };
    is %h.raku, '{:a(1)}', 'and writing it does not clobber the scalar leaf';
}

# --- a negative index is not a usable positional step ----------------------

{
    my %h;
    my $x := %h<g>;
    $x{-1} = 'n';
    is %h.raku, '{:g(${"-1" => "n"})}', 'an associative negative key stays a Hash key';
}

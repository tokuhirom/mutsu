use Test;

plan 10;

# A `Proxy` bound to an element mediates its own store: `@a[0] = 7` fires the
# `Proxy`'s STORE rather than replacing the container (ADR-0040 §9 seen from
# the destination side). The CHAINED spelling (`@a[$i][$j] = v`) used to miss
# that probe entirely -- every descent arm in
# `exec_index_assign_expr_nested_op_body` ends in a plain element-slot write,
# so the store overwrote the `Proxy` and the backing variable never moved
# (#8965). All four inner/outer container combinations are pinned, because the
# op reaches the destination slot through two different arms (array-outer via
# `Value::assign_element_slot`, hash-outer via `Value::hash_insert_through`).

{
    my @p;
    @p[0] = [0];
    my $backing = 0;
    @p[0][0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    @p[0][0] = 7;
    is $backing, 70, 'array-in-array: chained store fires the element Proxy STORE';
    is @p[0][0], 70, 'array-in-array: reading back goes through FETCH';
}

{
    my %h;
    %h<a> = {};
    my $backing = 0;
    %h<a><b> := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    %h<a><b> = 3;
    is $backing, 30, 'hash-in-hash: chained store fires the element Proxy STORE';
}

{
    my %g;
    %g<a> = [0];
    my $backing = 0;
    %g<a>[0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    %g<a>[0] = 4;
    is $backing, 40, 'array-in-hash: chained store fires the element Proxy STORE';
}

{
    my @r;
    @r[0] = {};
    my $backing = 0;
    @r[0]<k> := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v * 10 });
    @r[0]<k> = 5;
    is $backing, 50, 'hash-in-array: chained store fires the element Proxy STORE';
}

{
    # The other `:=` bind spelling: binding a VARIABLE whose own container is
    # the `Proxy` installs it one alias cell deeper, and the probe must unwrap
    # to the same mediating container.
    my @q;
    @q[0] = [0];
    my $backing = 0;
    my $px := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v + 1 });
    @q[0][0] := $px;
    @q[0][0] = 5;
    is $backing, 6, 'chained store finds a Proxy bound through an alias cell';
}

{
    # A `:=` bind INSTALLS a container; it must not be read as a store through
    # whatever Proxy the slot already holds.
    my @p;
    @p[0] = [0];
    my $first = 0;
    my $second = 0;
    @p[0][0] := Proxy.new(FETCH => -> $ { $first }, STORE => -> $, $v { $first = $v * 10 });
    @p[0][0] := Proxy.new(FETCH => -> $ { $second }, STORE => -> $, $v { $second = $v - 1 });
    @p[0][0] = 9;
    is $second, 8, 'a re-bind installs the new Proxy and the store reaches it';
    is $first, 0, 'the replaced Proxy no longer sees the store';
}

{
    # The assigned value itemizes exactly as the single-subscript twin's does,
    # so an aggregate RHS reaches STORE as one item.
    my @p;
    @p[0] = [0];
    my $backing = 0;
    @p[0][0] := Proxy.new(FETCH => -> $ { $backing }, STORE => -> $, $v { $backing = $v });
    @p[0][0] = [1, 2];
    is $backing.raku, '$[1, 2]', 'an aggregate RHS reaches STORE itemized';
}

{
    # The probe must not disturb an ordinary chained store.
    my @s;
    @s[1] = [0, 0];
    @s[1][1] = 42;
    is @s[1][1], 42, 'a plain chained element store is unaffected';
}

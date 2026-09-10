use v6;
use Test;

# Raku's signature binder puts a value bound to a plain `$`-sigiled parameter
# into a Scalar container: it is ONE element in list context and `.raku`
# shows the leading `$`. Sigilless (`\v`), `is raw`, and implicit-topic
# bindings stay raw. Verified against raku 2025.06 (see PR).

plan 24;

# --- for-loop parameter binding ---
{
    my @c = [<a b>], [<c d>];
    my @got;
    for @c -> $v { @got.push($v.raku) }
    is @got[0], '$["a", "b"]', 'for @c -> $v itemizes the element';
    is @got[1], '$["c", "d"]', 'for @c -> $v itemizes the second element';
}

{
    my @got;
    for [1,2], [3,4] -> $v { @got.push($v.raku) }
    is @got[0], '$[1, 2]', 'literal-list for -> $v itemizes too';
}

{
    my @c = [<a b>], [<c d>];
    my @got;
    for @c -> @row { @got.push(@row.raku) }
    is @got[0], '["a", "b"]', '@-sigil loop param binds the bare container';
}

{
    my @got;
    for ([1,2],) -> \v { @got.push(v.raku) }
    is @got[0], '[1, 2]', 'sigilless loop param binds raw';
}

{
    my @got;
    for ([1,2],) { @got.push(.raku) }
    is @got[0], '[1, 2]', 'implicit topic stays raw for a bare list element';
}

# --- multi-param loop binding (the CSV::Table t/5-save.t shape) ---
{
    my @c = [<a b>], [<c d>];
    my @got;
    for @c.kv -> $i, $v { @got.push($v.raku) }
    is @got[0], '$["a", "b"]', '.kv -> $i, $v itemizes the value param';
    is @got[1], '$["c", "d"]', '.kv -> $i, $v itemizes the second value';
}

{
    my @c = [<a b>], [<c d>];
    my @out;
    for @c.kv -> $i, $v { @out.push(sprintf "%-*.*s", 5, 5, $v) }
    is @out[0], 'a b  ', 'itemized param is ONE sprintf argument (no flatten)';
}

# --- sub/block parameter binding ---
{
    sub f($v) { $v.raku }
    is f([1,2]), '$[1, 2]', 'sub $ param itemizes an Array argument';
    is f((1,2)), '$(1, 2)', 'sub $ param itemizes a List argument';
    is f({:a(1)}), '${:a(1)}', 'sub $ param itemizes a Hash argument';
}

{
    sub r($v is raw) { $v.raku }
    is r([1,2]), '[1, 2]', 'is raw param binds the bare value';
}

{
    sub c($v is copy) { $v.raku }
    is c([1,2]), '$[1, 2]', 'is copy param itemizes';
}

{
    sub n(:$v) { $v.raku }
    is n(v => [1,2]), '$[1, 2]', 'named $ param itemizes';
}

{
    my $x = [9];
    sub w($v is rw) { $v.push(8) }
    w($x);
    is $x.raku, '$[9, 8]', 'is rw param still writes through to the caller';
}

# --- map/grep block parameters ---
{
    my @c = [1,2], [3,4];
    is @c.map(-> $v { $v.raku }).join('|'), '$[1, 2]|$[3, 4]',
        'map -> $v itemizes elements';
    is @c.map({ $^a.raku }).join('|'), '$[1, 2]|$[3, 4]',
        'placeholder $^a itemizes';
    is @c.grep(-> $v { $v.elems == 2 }).elems, 2,
        'grep -> $v still sees the array via methods';
}

# --- itemized invocant methods still iterate elements ---
{
    my $x = $[1,2];
    my $n = 0;
    for $x.cache { $n++ }
    is $n, 2, '.cache on an itemized array returns the plain array (deconts)';
}

# --- our/global scalar assignment ---
{
    our $g = [1,2];
    is $g.raku, '$[1, 2]', 'our $x = [...] itemizes like a my scalar';
}

# --- sigilless multi-params bind raw (roast S03-sequence/exhaustive.t shape) ---
{
    my @t = "d1", (1,3), "d2", (2,4);
    my @got;
    for @t -> \desc, \seed { @got.push(seed ~~ Array); @got.push(seed.elems) }
    is-deeply @got, [False, 2, False, 2],
        'sigilless multi-params bind the bare value (no itemize)';
}

# --- quanthash coercion deconts an itemized list operand ---
{
    my $r = <a b c d>;
    is (bag(<a b c e>) (-) $r).raku, '("e"=>1).Bag',
        'bag (-) itemized list subtracts the ELEMENTS';
    is ($r (-) bag(<a b c e>)).raku, '("d"=>1).Bag',
        'itemized list (-) bag contributes its elements';
}

done-testing;

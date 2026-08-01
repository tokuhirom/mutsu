use Test;

plan 31;

# Itemization is a property of the container a value sits in, not of the thing
# `>>` is walking, so a hyper goes straight through it. mutsu itemizes a
# non-Array container by wrapping it in a Scalar, which no container gate in the
# hyper matched: an itemized hash missed the Hash branch entirely and fell
# through to the generic element path, where an itemized value is ONE element.
# So `$g>>.Str` stringified the whole hash and wrapped it in a one-element list.
#
# The itemized-list twin was fixed earlier; this is the Hash (and QuantHash) one.
# Every assertion here also passes unmodified under rakudo.

# --- a hyper over an itemized hash maps its values and keeps the keys ---
{
    my %h = a => 1, b => 2;
    is (%h>>.Str).raku, '{:a("1"), :b("2")}', 'a plain hash hypers over its values';

    my $g = ${a => 1, b => 2};
    is ($g>>.Str).raku, '{:a("1"), :b("2")}', 'and so does an itemized hash literal';

    my $i = %h.item;
    is ($i>>.Str).raku, '{:a("1"), :b("2")}', 'and one itemized with .item';
}

# The result is a Hash, not a one-element list.
{
    my $g = ${a => 1, b => 2};
    my $r = $g>>.Str;
    is $r.elems, 2, 'the result has one entry per key';
    is-deeply $r.keys.sort.List, ('a', 'b'), 'keeping the original keys';
    ok $r ~~ Hash, 'and it is a Hash';
}

# --- with arguments ---
{
    my %r = a => "xy", b => "zw";
    my $s = %r.item;
    is ($s>>.substr(0, 1)).raku, '{:a("x"), :b("z")}',
        'a hyper with arguments maps the values too';
}

# --- the mutating postfix hypers write back through the itemization ---
{
    my %p = a => 1, b => 2;
    %p>>++;
    is %p.raku, '{:a(2), :b(3)}', 'a plain hash increments each value';

    my $q = ${x => 5, y => 6};
    $q>>++;
    is $q.raku, '${:x(6), :y(7)}', 'an itemized hash increments each value';

    my $d = ${x => 5};
    $d>>--;
    is $d.raku, '${:x(4)}', 'and decrements';
}

# --- an itemized QuantHash reached the same dead end ---
{
    my $s = <a b>.Set.item;
    is ($s>>.Str).raku, 'Set.new("a","b")', 'an itemized Set hypers over its elements';
    is ($s>>.Str).elems, 2, 'and answers one element per member';
}

# --- a Bag/Mix hyper maps the WEIGHTS and keeps the elements ---
#
# A QuantHash hypers exactly like a Hash: the method sees each weight, never the
# element, so `<a a b>.Bag>>.uc` is still `a => 2, b => 1` (`2.uc` is `"2"`,
# which coerces back to `2`). mutsu used to hand the method the whole `elem =>
# weight` Pair and then look for a weight that was no longer there, so every
# count came back 0 -- and a Mix, where a 0 weight means "not a member", came
# back empty.
{
    my $b = <a a b>.Bag;
    is-deeply ($b>>.Str), <a a b>.Bag, 'a Bag hyper keeps each element weight';
    is-deeply ($b>>.uc), <a a b>.Bag, 'and .uc never reaches the elements';
    is-deeply ($b.item>>.Str), <a a b>.Bag, 'an itemized Bag behaves identically';
    is-deeply ($b>>.succ), (a => 3, b => 2).Bag, 'a mapped weight is the new count';
    is-deeply ($b>>.pred), (a => 1).Bag, 'and a count that drops to 0 leaves the Bag';
    is-deeply ($b>>.&{ -1 }), Bag.new, 'as does a negative one';

    my $m = (a => 1.5, b => -2.5).Mix;
    is-deeply ($m>>.Str), (a => 1, b => -2).Mix,
        'an immutable Mix truncates the mapped weight to Int';
    is-deeply ($m.item>>.Str), (a => 1, b => -2).Mix, 'itemized or not';
    is-deeply ($m>>.&{ 0.4 }), Mix.new, 'a Mix weight of 0 drops the element';
    is-deeply ($m>>.&{ -1.5 }), (a => -1, b => -1).Mix, 'a negative one does not';

    # A MixHash keeps the full Real weight where the immutable Mix truncates.
    my $mh = (a => 1.5, b => 2.5).MixHash;
    is-deeply ($mh>>.abs), (a => 1.5, b => 2.5).MixHash, 'a MixHash keeps its Real weights';
    is ($mh>>.abs).WHAT.^name, 'MixHash', 'and the result is a MixHash';

    my $bh = <a a b>.BagHash;
    is-deeply ($bh>>.succ), (a => 3, b => 2).BagHash, 'a BagHash hyper yields a BagHash';
    is-deeply $bh, <a a b>.BagHash, 'leaving the original untouched';

    # A Set has no weight to map: the mapper sees 1 and the element survives
    # whenever the result is truthy.
    my $sh = <a b>.SetHash;
    is-deeply ($sh>>.Str), <a b>.SetHash, 'a SetHash keeps every truthy member';
    is-deeply ($sh>>.&{ False }), SetHash.new, 'and drops them all when falsy';

    # Non-Str elements keep their type through the rebuild.
    is-deeply ((1 => 2, 2 => 3).Bag>>.Str), (1 => 2, 2 => 3).Bag,
        'a Bag of Int elements keeps them Int';
}

# --- what must NOT change ---
{
    # The itemized-list rule is a kind flag, not a wrapper, and is untouched.
    my $pairs = $(:a(1), :b(2), :c(3));
    is-deeply ($pairs>>.key).List, ('a', 'b', 'c'), 'an itemized list still hypers into itself';

    # List-assignment flattening still counts an itemized list as one element.
    my @flat = $pairs;
    is @flat.elems, 1, 'while list assignment still sees one element';
}

use Test;

plan 14;

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
    # A Bag/Mix hyper drops the weights (`<a a b>.Bag>>.Str` is `("a"=>0,"b"=>0)`),
    # but that is not itemization: the plain form is equally wrong. Recorded in
    # todo/tickets/hyper-over-a-bag-or-mix-drops-the-weights.md.
    is ($s>>.Str).elems, 2, 'and answers one element per member';
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

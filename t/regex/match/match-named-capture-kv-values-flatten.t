use v6;
use Test;

# From Config::Netrc (ecosystem): a repeated top-level alternation of named
# subrules (`[<a>|<b>]*?`) stores each name's submatches as an Array on the
# Match — even a name matched zero or one time, since it is quantified by the
# `*?` around the alternation. `.kv` and `.values` must flatten that Array
# into the surrounding sequence, exactly like the already-correct handling
# for a quantified *positional* capture (`(x)*`) — `.pairs` and direct
# `<name>` access must NOT flatten, they keep the Array as one value.
#
# mutsu used to flatten only the positional branch of `.kv`/`.values` and
# push the raw Array for the named branch, so code iterating `$/.kv` saw a
# bare Array where raku hands it individual Match objects (or nothing, for a
# zero-match name) — surfacing as "Type Array does not support associative
# indexing" once that Array got treated as a loop element.

grammar G {
    token TOP { ^ [<a>|<b>]*? $ }
    token a { x }
    token b { y }
}

plan 14;

# Two matches for "a", one for "b": kv/values flatten both individually.
{
    my $m = G.parse("xxy");
    my @kv = $m.kv;
    is @kv.elems, 5, 'kv flattens named captures: 2*2 + 1 for a two-and-one match';
    is @kv.grep(* eq 'a').elems, 1, 'kv emits the "a" key once, not once per match';
    is @kv.grep({ $_ ~~ Match }).elems, 3, 'kv flattens all 3 named Match objects individually';

    my @vals = $m.values;
    is @vals.elems, 3, 'values flattens the two-and-one named Matches into 3 bare values';
    ok @vals.all ~~ Match, 'every flattened value is a Match';
}

# Exactly one match for "a": still flattens to a bare Match (not a
# one-element Array) in kv/values, matching the multi-match case above.
{
    my $m = G.parse("y");
    my @kv = $m.kv;
    is @kv.elems, 3, 'kv over a single "b" match and zero "a" matches has 3 elements';
    is @kv.grep(* eq 'a').elems, 1, 'the zero-match name still contributes its bare key';
    is @kv.grep(* eq 'b').elems, 1, 'the single-match name also contributes its own key';
    isa-ok @kv.first({ $_ ~~ Match }), Match, 'the single match flattens to a bare Match, not an Array';
}

# `.pairs` and direct `<name>` access must NOT flatten — the Array stays a
# single value there, unlike `.kv`/`.values`.
{
    my $m = G.parse("xxy");
    my @pairs = $m.pairs;
    is @pairs.elems, 2, 'pairs has one entry per name, not per match';
    my $a_pair = @pairs.first(*.key eq 'a');
    isa-ok $a_pair.value, Array, 'pairs keeps a multi-match named capture as one Array value';
    isa-ok $m<a>, Array, 'direct <name> access also keeps a multi-match capture as an Array';
}

# Iterating `for $m.kv -> $elem { ... }` over a zero-match name must not
# throw when probing an unrelated key — this is the exact failure mode from
# Config::Netrc's `sorting()` sub.
{
    my $m = G.parse("y");
    my $saw-str-elem = False;
    for $m.kv -> $elem {
        if $elem ~~ Str {
            $saw-str-elem = True;
            ok $elem<nonexistent>.defined === False, 'indexing an unrelated key on a Str elem from kv is a soft False, not a throw';
            last;
        }
    }
    ok $saw-str-elem, 'kv actually yielded a bare key Str to iterate over';
}

done-testing;

use Test;

# Every cursor a grammar parse mints is of the grammar's type from the start,
# not only once the parse finished (#12943): a code block, an assertion and a
# `.wrap`ped token's wrapper see `G`, not `Match`. A plain regex match inside
# the parse, on another string, stays a `Match`. Values are rakudo's.

plan 8;

{
    my @names;
    grammar G {
        token TOP { <d> { @names.push: $/.^name, $<d>.^name } <?{ @names.push: $<d>.^name; True }> }
        token d { \d }
    }
    ok G.parse('1'), 'parses';
    is-deeply @names, ['G', 'G', 'G'], 'code blocks and assertions see grammar cursors';
}

{
    my @inner;
    grammar H { token TOP { \d { @inner.push: ('x' ~~ /x/).^name } } }
    H.parse('1');
    is-deeply @inner, ['Match'], 'a match on another string stays a plain Match';
}

{
    my @seen;
    grammar W { token TOP { <num> }; token num { \d+ } }
    W.^find_method('num').wrap: sub (|c) { my $r := callsame; @seen.push: $r.^name; $r };
    my $m = W.parse('7');
    is-deeply @seen, ['W'], 'a wrapper sees a grammar cursor';
    is $m<num>.^name, 'W', 'the finished capture too';
}

{
    my @kept;
    grammar K { token TOP { <num> { @kept.push: $<num> } }; token num { \d+ } }
    my $m = K.parse('42');
    ok @kept[0] === $m<num>, 'a capture kept in mid-parse is the finished tree\'s';
    is @kept[0].^name, 'K', 'and reports the grammar type';
}

{
    my @names;
    grammar Inner { token TOP { \w+ } }
    grammar Outer { token TOP { (\w+) { @names.push: $/.^name; @names.push: Inner.parse(~$0).^name } } }
    Outer.parse('ab');
    is-deeply @names, ['Outer', 'Inner'], 'a nested parse of another grammar keeps its own type';
}

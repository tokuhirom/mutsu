use Test;

# ADR-11276 §9.114: `IO::Path::Parts` has a shape; `AT-POS`, `AT-KEY` and
# `flat` are its rows, `Any`'s list methods read it as one item, and
# `.hash`/`.Hash`/`.Map` read its parts by name (all as in Rakudo).

plan 17;

my $p = "/a/b.txt".IO.parts;

is $p[0].raku, ':volume("")', 'AT-POS 0 is the volume pair';
is $p[1].raku, ':dirname("/a")', 'AT-POS 1 is the dirname pair';
is $p[5].raku, 'Nil', 'AT-POS past the parts is Nil';
is $p<dirname>, '/a', 'AT-KEY reads a part';
is $p<nope>.raku, 'Nil', 'AT-KEY of no part is Nil';
is $p.flat.raku, '(:volume(""), :dirname("/a"), :basename("b.txt")).Seq', '.flat is the three pairs';

isa-ok $p.Map, Map, '.Map';
is $p.Hash<basename>, 'b.txt', '.Hash reads the parts by name';
is $p.hash<dirname>, '/a', '.hash reads the parts by name';

is $p.keys.raku, '(0,).Seq', '.keys: one item';
is $p.kv.elems, 2, '.kv: one item';
is $p.elems, 1, '.elems: one item';
is $p.List.elems, 1, '.List: one item';
isa-ok $p.Array, Array, '.Array';
is $p.Slip.elems, 1, '.Slip: one item';
is $p.cache.elems, 1, '.cache: one item';
is $p.pairs.elems, 1, '.pairs: one item';

use v6;
use Test;

plan 10;

# --- one-element Iterable trailing comma (PLAN §8.15) ------------------------
# Rakudo's List.raku rule: a sole element that istype Iterable gets a trailing
# comma so the round-trip does not flatten — including Iterable TYPE OBJECTS.
is [HyperSeq].raku, '[HyperSeq,]', 'Iterable type object gets the trailing comma';
is [List].raku, '[List,]', 'List type object gets the trailing comma';
is [Map].raku, '[Map,]', 'Map type object gets the trailing comma';
is [Hash,].raku, '[Hash,]', 'Hash type object keeps the trailing comma';
# QuantHashes are NOT Iterable in Rakudo; scalars never get the comma.
is [Set].raku, '[Set]', 'Set type object gets no comma';
is [Int].raku, '[Int]', 'non-Iterable type object gets no comma';
is [(1, 2),].raku, '[(1, 2),]', 'instance Iterable element keeps its comma';

# --- [Z] reduction returns a Seq (PLAN §8.15) --------------------------------
isa-ok ([Z] ((1, 2), (3, 4))), Seq, '[Z] reduction is a Seq';
is ([Z] ((1, 2), (3, 4), (5, 6))).raku, '((1, 3, 5), (2, 4, 6)).Seq',
    '[Z] over three lists zips n-ary and rakus with the .Seq suffix';
is-deeply ([Z] ((1, 2), (3, 4))).List, ((1, 3), (2, 4)), '[Z] values are the zipped tuples';

done-testing;

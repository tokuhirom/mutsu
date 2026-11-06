use Test;

# Found via the Configuration distribution (get-nodes: gather {..}.grep(..).duckmap(..)).
# A gather/lazy pipeline is an iterable receiver: the block sees its elements.

plan 8;

is (gather { take 1; take 2 }).duckmap({ $_ * 2 }).raku, '(2, 4)', 'duckmap on a gather';
is (gather { take 1; take 2 }).grep(* > 0).duckmap({ $_ * 2 }).raku, '(2, 4)',
    'duckmap on a grepped gather';
is (gather { take 1; take 2 }).grep(* > 0).duckmap({ $_ * 2 }).cache.raku, '(2, 4)',
    'duckmap on a grepped gather, then .cache';
is (gather { take 1; take 2 }).grep(* > 1).duckmap({ $_ * 2 }).raku, '(4,)',
    'duckmap result keeps only what grep let through';
is (gather { take 1; take 2 }).grep(* > 0).deepmap({ $_ * 2 }).raku, '(2, 4)',
    'deepmap on a grepped gather';
is (gather { take 1; take 2 }).grep(* > 0).nodemap({ $_ * 2 }).raku, '(2, 4)',
    'nodemap on a grepped gather';

role N { }
class C does N { has $.a }
sub gen(N:U $n) { "B" ~ $n.^name }
is (gather { take C }).grep({ $_ ~~ N }).duckmap({ "{ .^name }Builder" => gen $_ }).cache.raku,
    '(:CBuilder("BC"),)', 'duckmap block gets the type object, not the list';
is ((1, 2), 3).duckmap({ $_ ~~ Int ?? $_ * 2 !! Nil }).raku, '(Nil, 6)',
    'a plain list still descends only when the block rejects';

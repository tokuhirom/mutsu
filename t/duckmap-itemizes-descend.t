use v6;
use Test;

# When the block rejects an element, duckmap descends into it — and rakudo
# itemizes what comes back when the parent is a List/Seq/Hash, so the
# sublist is one element of the result rather than something that can
# flatten. A real Array parent does not itemize (same rule as deepmap).
# Expected values verified against raku.

plan 12;

my &b = -> Int $x { $x * 10 };

is (1, (2, 3)).duckmap(&b).raku, '(10, $(20, 30))',
    'a List descend under a List parent is itemized';
is (1, [2, 3]).duckmap(&b).raku, '(10, $[20, 30])',
    'an Array descend under a List parent is itemized';
is (1, (2, 3).Seq).duckmap(&b).raku, '(10, $(20, 30))',
    'a Seq descend comes back as an itemized List';
is (1, %(a => 2)).duckmap(&b).raku, '(10, ${:a(20)})',
    'a Hash descend under a List parent is itemized';
is (1, 2..3).duckmap(&b).raku, '(10, $(20, 30))',
    'a Range descend under a List parent is itemized';

is [1, (2, 3)].duckmap(&b).raku, '[10, (20, 30)]',
    'a List descend under an Array parent is NOT itemized';
is [1, [2, 3]].duckmap(&b).raku, '[10, [20, 30]]',
    'an Array descend under an Array parent is NOT itemized';
is [1, %(a => 2)].duckmap(&b).raku, '[10, {:a(20)}]',
    'a Hash descend under an Array parent is NOT itemized';

is (1, (2, (3, 4))).duckmap(&b).raku, '(10, $(20, $(30, 40)))',
    'itemization applies at every List nesting level';
is %(a => 1, b => (2, 3)).duckmap(&b).raku, '{:a(10), :b($(20, 30))}',
    'a descend in a Hash value position is itemized';
is (1, (2, 3)).Seq.duckmap(&b).raku, '(10, $(20, 30))',
    'a Seq parent itemizes its element descends';
is (1, (2, 3)).Seq.duckmap(&b).WHAT.gist, '(List)',
    'duckmap on a Seq returns a List';

done-testing;

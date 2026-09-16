use v6;
use Test;

# `.sort`/`.join` on a Uni/NFC/NFD/NFKC/NFKD value decompose it into its OWN
# codepoints, each a plain Int -- the same idiom `.map`/`.grep`/`.first`
# already got fixed for (#8517, uni-map-grep-decompose-codepoints.t).
#
# `.sort` fell through to the generic "any non-list value sorts as a
# one-element list of itself" fallback (`sort_value_generic`,
# src/runtime/methods_collection_ops/sort.rs), so `'ba'.NFC.sort` answered a
# one-element Seq holding the whole NFC value instead of its sorted
# codepoints. `.join` had no native dispatch entry at all for a bare Uni
# receiver (`dispatch_1arg.rs`), so it died with "No such method 'join' for
# invocant of type 'NFC'" (issue #8532).

plan 7;

my $n = 'ba'.NFC;
is $n.sort.join(','), '97,98',
    '.sort decomposes an NFC value into codepoints, then sorts them';
is $n.join(','), '98,97',
    '.join decomposes an NFC value into codepoints, in their original order';

is 'hello'.NFC.sort.join(','), '101,104,108,108,111',
    '.sort on a multi-codepoint NFC value';
is 'hello'.NFD.join('-'), '104-101-108-108-111',
    '.join on an NFD value';

is ''.NFC.sort.join(','), '',
    '.sort on an empty NFC value stays empty';
is 'a'.NFC.join(','), '97',
    '.join on a single-codepoint NFC value (a one-element fallback would look correct here)';

is 'cba'.NFC.sort({ $^b <=> $^a }).join(','), '99,98,97',
    '.sort with an explicit comparator still decomposes the NFC receiver first';

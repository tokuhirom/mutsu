# `.gist` of an aggregate shows at most the first 100 elements, then ` ...`
# (Rakudo caps aggregate gists so a huge array/list does not flood output).
# `.raku`, `.elems`, and the actual contents are unaffected.
use Test;

plan 12;

is [1, 2, 3].gist, '[1 2 3]', 'small array gist is full';
is [1 .. 100].gist.words.tail, '100]', '100-element array gist is not truncated';
is [1 .. 101].gist.words.tail, '...]', '101-element array gist truncates with ...';
is [1 .. 102].gist, [1 .. 101].gist, '102 and 101 gist the same (both capped)';
is [1 .. 1000].gist.words.tail, '...]', '1000-element array gist truncates';
is [1 .. 101].gist.words.elems, 101, 'truncated gist has 100 elems + "..."';

is (1 .. 101).List.gist.words.tail, '...)', 'List gist truncates with ...';
is (1 .. 101).Seq.gist.words.tail, '...)', 'Seq gist truncates with ...';

# A Range gists as range notation, unaffected by the element cap.
is (1 .. 101).gist, '1..101', 'Range gist is range notation, not truncated';

# The cap is display-only: length and contents are intact.
is [1 .. 1000].elems, 1000, '.elems is not affected by the gist cap';
is [1 .. 1000][500], 501, 'indexing past the cap still works';
is [1 .. 3].gist, [1, 2, 3].gist, 'range- and list-built arrays gist alike';

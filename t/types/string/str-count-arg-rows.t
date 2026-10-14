use Test;

# ADR-11276 §9.102: `words($limit)`, `lines($limit)`, `lines(:chomp, :count)`
# and `chop($n)` are rows of `Str` and `Cool`. A `Cool` receiver reads its
# string form; the count is read as `.Int` (a `Str` parses, a rational
# truncates, an allomorph reads its number), and `*`/`Inf` cap nothing.

plan 18;

is-deeply "a b c".words(2).List, ('a', 'b'), 'words($n)';
is-deeply "a b c".words(*).List, ('a', 'b', 'c'), 'words(*) caps nothing';
is-deeply "a b c".words(Inf).List, ('a', 'b', 'c'), 'words(Inf) caps nothing';
is-deeply "a b c".words(1.5).List, ('a',), 'a rational count truncates';
is-deeply "a b c".words("2").List, ('a', 'b'), 'a string count parses';
is-deeply "a b c".words(<2>).List, ('a', 'b'), 'an allomorph count reads its number';
is-deeply (1..3).words(2).List, ('1', '2'), 'a Range reads its string form';
is-deeply 123.words(1).List, ('123',), 'an Int reads its string form';

is-deeply "a\nb\nc".lines(2).List, ('a', 'b'), 'lines($n)';
is-deeply "a\nb\n".lines(:!chomp).List, ("a\n", "b\n"), 'lines(:!chomp)';
is "a\nb\n".lines(:count), 2, 'lines(:count) counts';
is-deeply "a\nb\nc".lines(1, :!chomp).List, ("a\n",), 'lines($n, :!chomp)';
is <1e0>.lines(<1>).join('|'), '1e0', 'an allomorph receiver reads its string';

is "abcd".chop(2), 'ab', 'chop($n)';
is "ab".chop(5), '', 'chop past the start is empty';
is 1234.chop(1), '123', 'an Int reads its string form';
is "abcd".chop(1.9), 'abc', 'a rational count truncates';

throws-like { (a => 'b c').words(1) }, X::Method::NotFound, 'a Pair is not Cool';

use Test;

# An unanchored scan whose pattern starts with a `X+` run skips the starts a
# failed start's run makes futile (#10315). The skipped starts must not hide a
# match: each expectation was checked against rakudo.

plan 16;

sub m($s, $r) { my $m = $s ~~ $r; $m ?? "{$m.from}-{$m.to}:{$m}" !! 'no' }

is m("the quick brown fox 12345 jumps over 67-8 lazy\n" x 3, /\w+ \s \d ** 6/), 'no', 'no run of six digits';
is m("the quick brown fox 123456 jumps", /\w+ \s \d ** 6/), '16-26:fox 123456', 'a word then six digits';
is m("aaaa bbbb cccc 1234567", /\w+ \s \d ** 6/), '10-21:cccc 123456', 'the last word before the digits';
is m("é1 ü2 abc 1234567", /\w+ \s \d ** 6/), '6-16:abc 123456', 'non-ASCII words before it';
is m("word word word end", /\w+ \s end/), '10-18:word end', 'a literal tail';
is m("wo\x[301]rd word 77", /\w+ \s \d \d/), '5-12:word 77', 'a combining mark inside a skipped run';
is m("x" x 50 ~ " 9", /\w+ \s \d/), '0-52:' ~ "x" x 50 ~ ' 9', 'one long run';
is m("ab ab ab abc", /\w+ ' ' abc/), '6-12:ab abc', 'a quoted-space tail';
is m("ab ab ab abc", /[ab]+ \s abc/), '6-12:ab abc', 'a class run';
is m("--- a-b 12", /\w+ \s \d/), '6-9:b 1', 'a run stopped by punctuation';
is m("tab\there 99", /\w+ \s \d/), '4-10:here 9', 'a TAB after the run';
is m("line one\nline 22\n", /\w+ \n \w+ \s \d/), "5-15:one\nline 2", 'a newline in the middle';
is m("aaa", /a+ b/), 'no', 'a run with no tail';
is m("", /\w+ \s/), 'no', 'an empty subject';
is ("ab cd ef 12".match(/\w+ \s/, :g)).map(~*).join('|'), 'ab |cd |ef ', 'a global match';
is ("ab cd ef 12".match(/\w+ \s/, :ov)).map(~*).join('|'), 'ab |b |cd |d |ef |f ', 'overlapping matches try every start';

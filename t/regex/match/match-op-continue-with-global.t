use Test;

# `m:c(N)` / `m:pos(N)` start the scan at N together with `:g`, `:ov`, `:ex`,
# `:x` and `:nth`; `:pos` anchors only the first match (#18886).

plan 11;

is ("abc" ~~ m:c(1):g/./).map(~*).join, 'bc', ':c with :g';
is ("abcabc" ~~ m:c(2):g/b/).map(*.from).join(','), '4', ':c skips an earlier match';
is ("abc" ~~ m:p(1):g/./).map(~*).join, 'bc', ':p with :g';
is ("abcabc" ~~ m:p(0):g/c/).elems, 0, ':p with no match at the position';
is ("abcabc" ~~ m:pos(2):g/c/).map(*.from).join(','), '2,5', ':pos anchors only the first match';
is ("abcd" ~~ m:c(1):ov/./).map(~*).join, 'bcd', ':c with :ov';
is ("abcd" ~~ m:c(1):ex/./).map(~*).join, 'bcd', ':c with :ex';
is ("abcd" ~~ m:c(1):x(2)/./).map(~*).join, 'bc', ':c with :x';
is ("abcd" ~~ m:c(1):nth(2)/./).Str, 'c', ':c with :nth';
my $n = 2;
is ("abcabc" ~~ m:c($n):g/b/).map(*.from).join(','), '4', 'an expression argument';
is ("abc" ~~ m:c(5):g/./).elems, 0, 'a start past the end finds nothing';

use v6;
use Test;

# From SION (zef distribution): its lexer walks a text with `m:p($pos)/.../`
# and `$/.to`. "\r\n" is one grapheme, so `:p(N)`/`:c(N)` count graphemes and
# `.from`/`.to` of the resulting Match must too (they were code-point offsets).
plan 15;

my $t = "a\r\nb c";
$t ~~ m:p(1)/\s/;
is-deeply ($/.from, $/.to), (1, 2), ':p(1) over a CRLF';
$t ~~ m:p(1)/\s+/;
is-deeply ($/.from, $/.to), (1, 2), ':p(1) with a quantifier';
$t ~~ m:c(1)/b/;
is-deeply ($/.from, $/.to), (2, 3), ':c(1) search';
ok $t ~~ m:p(2)/b/, ':p(2) is the grapheme after the CRLF';
is-deeply ($/.from, $/.to), (2, 3), 'its offsets';
nok $t ~~ m:p(3)/b/, ':p(3) is past it';

my $s = "a :\r\n 1 ]";
ok $s ~~ m:p(1)/\s+/, 'leading space';
is $/.to, 2, '.to after a space';
ok $s ~~ m:p($/.to)/':'/, 'resume at $/.to';
is $/.to, 3, '.to after the colon';
ok $s ~~ m:p($/.to)/\s+/, 'whitespace including the CRLF';
is $/.to, 5, '.to counts the CRLF as one';
ok $s ~~ m:p($/.to)/'1'/, 'next token is where .to says';
is $/.to, 6, 'and its end';
is $/.orig, $s, '.orig is the subject';

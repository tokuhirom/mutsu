use v6;
use Test;

plan 14;

# Raku never allows `(` as a quote/regex delimiter directly after the
# construct name or its adverbs — that is call syntax (`q(...)` calls `q`).
# A paren delimiter requires intervening whitespace: `q (...)`.

# A user routine whose name collides with a quote construct is callable
# with call parens. (Each declaration is scoped to its own block, since a
# declared routine shadows the quote construct for the rest of the scope.)
{
    sub qq($x) { $x * 2 }
    is qq(5), 10, 'qq(5) calls the user sub qq, not a qq() quote';
}
{
    multi mm($x) { 'one' }
    multi mm($x, $y) { "two:$y" }
    is mm(9), 'one', 'mm(9) calls the user multi mm, not an m:sigspace regex';
    is mm(1, 2), 'two:2', 'mm(1, 2) dispatches with both args';
}
{
    sub tr($x) { $x + 1 }
    is tr(5), 6, 'tr(5) calls the user sub tr, not a transliteration';
}
{
    sub s($x) { $x ~ '!' }
    is s('hi'), 'hi!', 's("hi") calls the user sub s, not a substitution';
}

# The paren-without-space forms are compile errors when no such routine
# exists (never silently a quote).
ok !try { EVAL 'q(foo)'; True }, 'q(foo) is not a q-quote';
ok !try { EVAL 'qw(a b)'; True }, 'qw(a b) is not a word quote';
ok !try { EVAL 'my $x = qq(foo); True' }, 'qq(foo) is not a qq-quote';

# With whitespace the paren IS a valid delimiter.
is q (foo bar), 'foo bar', 'q (foo bar) — space before paren delimiter';
my $lang = 'Raku';
is qq (hello $lang), 'hello Raku', 'qq (...) interpolates';
is q (($lang)), '$lang', 'q ((...)) — doubled parens after space';

$_ = '9';
is (m (9)).Str, '9', 'm (9) — space before paren delimiter matches';
$_ = 'A';
is (m:i (a)).Str, 'A', 'm:i (a) — space after adverb, paren delimiter';

# Other delimiters right after the name still work, no space needed.
is q{hi} ~ qq[there], 'hithere', 'brace/bracket delimiters unaffected';

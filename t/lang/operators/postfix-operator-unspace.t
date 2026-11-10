use Test;

# `\ ` (unspace) before a declared postfix operator, symbolic or a word.
# Identical under rakudo.
plan 4;

multi sub postfix:<§>(Mu $s) { "sym:$s" }
multi sub postfix:<data-root>(Mu $s) { "word:$s" }

is (3\ §), 'sym:3', 'unspace before a symbolic postfix';
is (3\ data-root), 'word:3', 'unspace before a word postfix';
my %h = a => 1;
is (%h\ data-root), 'word:a	1', 'on a variable';
is ({ %(b => 2)\ data-root }()), 'word:b	2', 'inside a block';

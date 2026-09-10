use v6;
use Test;

# The `**` range quantifier accepts an explicit greediness marker:
# `**?` is frugal (non-greedy) and `**!` is the explicit greedy marker
# (identical to a bare `**`, just written out). Regression: mutsu only
# recognized `**?` and choked on `**!` with "Unrecognized regex
# metacharacter !". (Language/regexes.rakudoc)

plan 6;

# Frugal vs explicit-greedy on a bracketed range quantifier.
is ~('/foo/o/bar/' ~~ /\/.**?{1..10}\//), '/foo/', '**?{1..10} is frugal (minimal)';
is ~('/foo/o/bar/' ~~ /\/.**!{1..10}\//), '/foo/o/bar/', '**!{1..10} is greedy (maximal)';

# `**!` matches the same as a bare `**` (both greedy).
is ~('aaaa' ~~ /a**!{1..3}/), 'aaa', '**!{1..3} greedily takes 3';
is ~('aaaa' ~~ /a**{1..3}/),  'aaa', 'bare **{1..3} also greedy (baseline)';

# The marker also works on a plain count and a non-brace range.
is ~('aaaa' ~~ /a**!2/),      'aa',  '**!N works on an exact count';
is ~('aaaaa' ~~ /a**!2..4/),  'aaaa', '**!N..M works on a bare range';

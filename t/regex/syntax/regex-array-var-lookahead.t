use v6;
use Test;

# `<?@var>` / `<!@var>` are the zero-width lookahead forms of the `<@var>`
# array-variable subrule: they assert (positively / negatively) that the
# current position matches one of the array's elements without consuming
# input. Regression: mutsu only handled the consuming `<@var>` form, so
# `<?@var>` fell through to an unknown assertion and never matched.
# (Language/regexes.rakudoc)

plan 7;

my @ending_letters = <d e f>;

ok 'abcdefg' ~~ rx{ abc <?@ending_letters> }, '<?@var> matches when next is in the array';
is ~($/), 'abc', '<?@var> is zero-width (consumes nothing)';
nok 'abcxyz' ~~ rx{ abc <?@ending_letters> }, '<?@var> fails when next is not in the array';

nok 'abcdefg' ~~ rx{ abc <!@ending_letters> }, '<!@var> fails when next is in the array';
ok 'abcxyz' ~~ rx{ abc <!@ending_letters> }, '<!@var> matches when next is not in the array';

# The consuming form still works (unchanged behavior).
ok 'abcdefg' ~~ rx{ abc <@ending_letters> }, '<@var> still consumes a matching element';
is ~($/), 'abcd', '<@var> consumed the matched element';

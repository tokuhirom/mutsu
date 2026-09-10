use v6;
use Test;

plan 6;

# A `{ ... }` block interpolation scans for its matching close brace. Braces that
# appear inside a quoted string *within* the block must not count, or the scan
# unbalances and the string looks unterminated ("couldn't find final '\"'") or
# closes early. Regression: mutsu counted every `{`/`}` literally.

is "a{ '{' }b", 'a{b', 'literal { inside a single-quoted string in interpolation';
is "a{ '}' ~ 'x' }b", 'a}xb', 'literal } inside a single-quoted string in interpolation';
is "[{ "}" }]", '[}]', 'literal } inside a nested double-quoted string';
is "n={ '{}{}' .chars }", 'n=4', 'multiple literal braces in a quoted string';

# The real-world trigger: a regex carrying a quoted brace inside interpolation
# (from Data::Dump's `$obj.raku.subst(/'{' .+? $/, '')`).
my $s = 'abc{def';
is "[{ $s.subst(/'{' .+? $/, '') }]", '[abc]', 'quoted brace inside a regex inside interpolation';

# A genuine nested code block (balanced braces) still works.
my $n = 3;
is "v={ $n > 0 ?? { "pos" }() !! "neg" }", 'v=pos', 'nested balanced code block still interpolates';

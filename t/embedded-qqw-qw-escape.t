use v6;
use Test;

# The embedded quote-words escapes usable inside an otherwise-non-interpolating
# string: the qqw form interpolates then word-splits; the qw form word-splits a
# literal body. In string context the word list joins with single spaces.
# Previously mutsu left them literal (or only handled the qq form).

plan 8;

# The doc example (Language/quoting.rakudoc): single-quoted string.
{
    my $animal = "quaggas";
    is 'These animals are \qqw[$animal or zebras]',
        'These animals are quaggas or zebras',
        'single-quote qqw interpolates and word-splits';
}

# qw keeps the body literal (no interpolation), just word-splits.
{
    my $a = "x";
    is 'no interp \qw[$a y]', 'no interp $a y', 'single-quote qw does not interpolate';
}

# Works inside q{...} the same as inside '...'.
{
    my $a = "x";
    is q{a \qqw[$a y z] b}, 'a x y z b', 'q-braces qqw interpolates';
    is q{a \qw[one two] b}, 'a one two b', 'q-braces qw word-splits';
}

# Existing qq form still works.
{
    my $a = "x";
    is q{a \qq[$a] b}, 'a x b', 'q-braces qq still works';
}

# Q{...} is fully raw — the escape stays literal. (Build the expected value
# with Q so the backslash sequence is not itself processed.)
{
    is Q{raw \qw[a b]}, Q{raw \qw[a b]}, 'Q-braces leaves the escape literal';
    ok Q{raw \qw[a b]}.contains(Q{\qw}), 'Q-braces output still contains the raw escape';
}

# Angle delimiter form.
{
    is 'x \qw<one two three> y', 'x one two three y', 'qw with angle delimiter';
}

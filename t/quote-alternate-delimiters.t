use Test;

plan 27;

# Doubled bracket delimiters
is q{{ab}},        'ab',          'q{{ab}} — doubled braces';
is q{{ fo} }},     ' fo} ',       'q{{ fo} }} — single } inside doubled braces';
is q{{ {{ } }} }}, ' {{ } }} ',   'q{{ {{ } }} }} — nested doubled braces';

# Doubled brackets
is q[[$foo $bar]], '$foo $bar',    'q[[$foo $bar]] — doubled brackets';
is q[[ fo] ]],    ' fo] ',        'q[[ fo] ]] — single ] inside doubled brackets';

# Doubled parentheses
is q(($foo $bar)), '$foo $bar',    'q(($foo $bar)) — doubled parens';

# Q with doubled brackets
is Q{{hello}},     'hello',        'Q{{hello}} — doubled braces';
is Q[[$foo]],      '$foo',         'Q[[$foo]] — doubled brackets';

# Space before delimiter
is q (foo bar),    'foo bar',      'q (foo bar) — space before paren';
is Q {\\},         '\\\\',         'Q {..} — space before brace';
is q {hello},      'hello',        'q {hello} — space before brace';

# Q:q and Q:qq adverbs
{
    my $foo = "FOO";
    my $bar = "BAR";
    is Q:q/$foo $bar/,  '$foo $bar', 'Q:q does not interpolate';
    is Q:qq/$foo $bar/, 'FOO BAR',   'Q:qq does interpolate';
}

# qw — quote words
{
    my @q = qw/foo bar baz/;
    is +@q, 3, 'qw// produces a list';
    is @q[0], 'foo', 'qw first element';
    is @q[1], 'bar', 'qw second element';
    is @q[2], 'baz', 'qw third element';
}

# q:w
{
    my @q = q:w/foo bar/;
    is +@q, 2, 'q:w produces a list';
    is @q[0], 'foo', 'q:w first element';
}

# q :w (space before adverb)
{
    my @q = q :w /foo bar/;
    is +@q, 2, 'q :w produces a list';
    is @q[0], 'foo', 'q :w first element';
}

# qq:w
{
    my $foo = "FOO";
    my $bar = "BAR";
    my @q = qq:w/$foo $bar/;
    is +@q, 2, 'qq:w produces a list';
    is @q[0], 'FOO', 'qq:w interpolates and splits';
    is @q[1], 'BAR', 'qq:w second element';
}

# 0d decimal prefix
is 0d42, 42, '0d42 decimal prefix';
is 0d0,  0,  '0d0 decimal prefix';
is 0D10, 10, '0D10 decimal prefix (uppercase)';

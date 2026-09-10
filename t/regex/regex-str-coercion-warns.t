use Test;
use lib 'roast/packages/Test-Helpers/lib';
use Test::Util;

plan 14;

# raku refuses to hand back a regex's source text in string context: it warns
# and yields the EMPTY string. `.gist`/`.raku` legitimately show the source.

is (quietly (/a/).Str), '', '.Str of a Regex is the empty string';
is (quietly (/a/).Stringy), '', '.Stringy too';
is (/a/).gist, '/a/', '.gist still shows the source text';
is (/a/).raku, '/a/', '.raku too';
is (quietly ~(/a/)), '', 'prefix ~ is the empty string';
is (quietly ("x" ~ /a/)), 'x', 'infix ~ contributes nothing';
is (quietly "x{/a/}y"), 'xy', 'interpolation contributes nothing';
{
    my $r = /a/;
    is (quietly "x$r"), 'x', 'and through a variable';
}

# The coercion is what makes a Regex-against-Regex smartmatch fail: `""`
# cannot contain an `a`, while the source text `"/a/"` does.
is-deeply (quietly (/a/ ~~ /a/)), Nil, 'Regex ~~ Regex is Nil';
{
    my $r = /a/;
    is-deeply (quietly ($r ~~ $r)), Nil, '... through variables too';
}
{
    my @seen;
    quietly { for (/a/,) { @seen.push(($_ ~~ $_).raku) } }
    is-deeply @seen.List, ('Nil',), 'a regex-valued topic self-matches to Nil';
}

# The warning is real, goes to stderr, and `quietly` suppresses it.
is_run 'say (/a/).Str.raku',
    { out => "\"\"\n", err => /'Regex object coerced to string'/, status => 0 },
    'the coercion warns on stderr';
is_run 'quietly say (/a/).Str.raku',
    { out => "\"\"\n", err => '', status => 0 },
    'and quietly suppresses it';

# `say` uses .gist and does NOT warn.
is_run 'say /a/',
    { out => "/a/\n", err => '', status => 0 },
    'say renders the source text with no warning';

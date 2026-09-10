use v6;
use Test;
use JSON::Fast;

# Strict JSON grammar in native from-json (JSON::Fast t/01-parse.t parity).
# Note: \u escape sequences are assembled from chr(92) so this file stays ASCII
# and no host-language escape processing can mangle them.

my $bs = 92.chr;
sub u(*@cps) { @cps.map({ $bs ~ 'u' ~ $_ }).join('') }

# Surrogate pairs decode to the combined code point.
{
    my $r = from-json('["' ~ u('D801', 'DC37') ~ '"]');
    is $r[0].ord, 0x10437, 'surrogate pair decodes to astral char';
    is $r[0].chars, 1, 'pair produces a single character';
}
{
    my $r = from-json('["' ~ u('D83D', 'DE39') ~ u('D83D', 'DC8D') ~ '"]');
    is $r[0].ords.list, (0x1F639, 0x1F48D), 'two consecutive pairs decode';
}

# Lone / mismatched surrogates are rejected.
for 'DFAA', 'D800' -> $cp {
    dies-ok { from-json('["' ~ u($cp) ~ '"]') }, "lone surrogate $cp rejected";
}
dies-ok { from-json('["' ~ u('DD1E', 'D834') ~ '"]') },
    'low-then-high surrogate order rejected';
dies-ok { from-json('["' ~ u('D800') ~ 'abc"]') },
    'high surrogate followed by plain text rejected';

# Invalid backslash escapes are rejected.
dies-ok { from-json('["' ~ $bs ~ 'a"]') }, 'backslash-a escape rejected';
dies-ok { from-json('["' ~ $bs ~ 'x15"]') }, 'backslash-x escape rejected';

# Raw control characters inside strings are rejected.
dies-ok { from-json('["' ~ 9.chr ~ '"]') }, 'raw tab in string rejected';
dies-ok { from-json('["' ~ 10.chr ~ '"]') }, 'raw newline in string rejected';

# Escaped control characters still parse.
is from-json('["' ~ $bs ~ 't"]')[0], "\t", 'escaped tab parses';
is from-json('["' ~ u('0000') ~ '"]')[0], 0.chr, 'escaped NUL parses';

# Numbers require a digit after the decimal point.
for '[1.]', '[-2.]', '[0.e1]', '[2.e3]' -> $t {
    dies-ok { from-json($t) }, "malformed number $t rejected";
}
is-deeply from-json('[1.5, -0.25, 2e3]'), $[1.5, -0.25, 2e3],
    'well-formed numbers still parse';

done-testing;

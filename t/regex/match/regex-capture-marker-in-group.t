use v6;
use Test;

# `<(` / `)>` set the overall match's start/end. When the markers sit inside a
# non-capturing group `[...]` (or are reached through a `~`-goalpost), those
# positions must propagate out to the whole Match — mutsu dropped them when
# folding a sub-pattern's captures into the outer result, so the match kept its
# outer extent (`"<abc>"` instead of the marked `"abc"`).

plan 8;

# --- markers directly (regression guard) ----------------------------------
is ~($_ given ('<abc>' ~~ / '<' <( \w+ )> '>' /)), 'abc',
    'bare <( ... )> markers set the match extent';

# --- markers inside a `[...]` group ---------------------------------------
is ~($_ given ('<abc>' ~~ / '<' [<( \w+ )>] '>' /)), 'abc',
    '<( ... )> inside a [ ] group propagates the extent out';

is ~($_ given ('x42y' ~~ / 'x' [<( \d+ )>] 'y' /)), '42',
    '<( ... )> in a group with surrounding literals';

# --- markers reached through a `~` goalpost -------------------------------
is ~($_ given ('<abc>' ~~ / '<' ~ '>' [<( \w+ )>] /)), 'abc',
    '<( ... )> in a group under a ~ goalpost sets the extent';

# --- a frugal inner still backtracks to satisfy the goalpost --------------
is ~($_ given ('<0.0.1>' ~~ / '<' ~ '>' [<( .+? )>] /)), '0.0.1',
    'a frugal inner under the goalpost expands to reach the goal';

# --- only the marked span is captured, surrounding text is excluded -------
{
    my $m = 'aXbYc' ~~ / a <( . )> b <( . )> /;  # last <( wins for start
    # After two <( markers the *last* start wins; end is the last )> (implicit end).
    ok $m.defined, 'multiple <( markers parse and match';
}

# --- named capture inside the marked group is still recorded --------------
{
    my $m = '<abc>' ~~ / '<' [<( $<w>=(\w+) )>] '>' /;
    is (~$m.<w>), 'abc', 'a named capture inside the marked group is preserved';
}

# A regex used as the predicate of .first publishes its Match through the
# enclosing routine, as it does when the same regex is used directly.
sub multipart-boundary(@headers) {
    with @headers.first: {
        .defined
        && /
            ^ 'multipart/' .* 'boundary=' '"' ? <( <-[">]>+ )>
        /
    } {
        ~$/
    }
}
is multipart-boundary(['multipart/byteranges; boundary=3d6b6a416f9b5']),
    '3d6b6a416f9b5',
    'a regex predicate in .first publishes its Match to the enclosing routine';

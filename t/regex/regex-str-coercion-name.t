use Test;

# Str-coercing a Regex warns and yields the regex's NAME, not its source and
# not the empty string: `~&foo` is "foo" for a `regex`/`token`/`rule` routine
# and "" for an anonymous `/.../` or `rx/.../`. Measured against raku.
#
# The named half is what lets two regex routines be compared as strings — the
# vendored Test's `is` does `$got eq $expected` — which used to report every
# named regex as equal to every other one (roast S02-magicals/sub.t,
# "&?ROUTINE is correct inside a regex").

plan 10;

my regex foo { a }
my token tok { b }
my rule  rul { c }

my ($sfoo, $stok, $srul, $smeth, $anon, $anonadv, $cat, $interp, $ne, $eq);
quietly {
    $sfoo    = ~&foo;
    $stok    = ~&tok;
    $srul    = ~&rul;
    $smeth   = &foo.Str;
    $anon    = ~rx/x/;
    $anonadv = ~rx:i/x/;
    $cat     = '<' ~ &foo ~ '>';
    $interp  = "[{ ~&foo }]";
    $ne      = &foo ne &tok;
    $eq      = &foo eq &foo;
}

is $sfoo,  'foo', 'Str-coercing a named regex yields its name';
is $stok,  'tok', 'Str-coercing a named token yields its name';
is $srul,  'rul', 'Str-coercing a named rule yields its name';
is $smeth, 'foo', '.Str agrees with the coercion';

is $anon,    '', 'Str-coercing an anonymous rx yields the empty string';
is $anonadv, '', 'Str-coercing an adverbed anonymous rx yields the empty string';

is $cat,    '<foo>', 'concatenation uses the same coercion';
is $interp, '[foo]', 'interpolation uses the same coercion';

# Two DIFFERENT named regexes must not compare equal as strings.
ok $ne, 'two different named regexes are not eq';
ok $eq, 'the same named regex is eq to itself';

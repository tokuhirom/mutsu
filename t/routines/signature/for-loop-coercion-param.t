use Test;

# A `for` loop's pointy parameter with a coercion type coerces each item, the
# way a signature parameter does. Found via the GeoIP2 distribution, which
# walks regex captures with `for $/[0] -> Int( ) $octet { $octet.polymod(...) }`.

plan 7;

my @seen;
for '3', '4' -> Int() $o { @seen.push: $o }
is-deeply @seen, [3, 4], 'Int() coerces each Str item';

@seen = ();
for '5' -> Int( ) $o { @seen.push: $o.^name }
is-deeply @seen, ['Int'], 'the spaced Int( ) spelling coerces too';

@seen = ();
'1.22.3' ~~ / ^ (\d+) ** 3 % '.' $ /;
for $/[0] -> Int() $octet { @seen.push: $octet.polymod(10).join(",") }
is-deeply @seen, ["1,0", "2,2", "3,0"], 'regex captures coerce to Int';

@seen = ();
for '6' -> Int(Str) $o { @seen.push: $o.^name }
is-deeply @seen, ['Int'], 'Int(Str) coerces a Str item';

@seen = ();
for 7 -> Int(Str) $o { @seen.push: $o }
is-deeply @seen, [7], 'Int(Str) accepts an item that is already an Int';

throws-like { for 3.5 -> Int(Str) $o { } }, X::TypeCheck::Binding::Parameter,
    'Int(Str) rejects an item of neither type';

throws-like { for '3' -> Int $o { } }, X::TypeCheck::Binding::Parameter,
    'a plain type constraint still only checks';

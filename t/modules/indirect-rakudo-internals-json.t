use v6;
use Test;

# `Rakudo::Internals::JSON` (and `Rakudo::Internals`) must resolve through the
# indirect symbolic-name lookup `::("…")`, not just as a direct bareword. zef's
# `Zef::from-json`/`to-json` use `::("Rakudo::Internals::JSON").from-json($text)`;
# mutsu previously failed that with "No such symbol 'Rakudo::Internals::JSON'"
# even though the direct form already worked.

plan 4;

my $j = ::("Rakudo::Internals::JSON");
ok $j ~~ Rakudo::Internals::JSON, 'indirect lookup yields the JSON package';

is ::("Rakudo::Internals::JSON").from-json('{"a":1,"b":2}')<b>, 2,
    'from-json via indirect ::() lookup';

like ::("Rakudo::Internals::JSON").to-json({ x => 1 }), /'"x"'/,
    'to-json via indirect ::() lookup';

# `Rakudo::Internals` itself also resolves indirectly.
ok ::("Rakudo::Internals").defined.not || ::("Rakudo::Internals") ~~ Rakudo::Internals,
    'indirect lookup of Rakudo::Internals resolves to the package';

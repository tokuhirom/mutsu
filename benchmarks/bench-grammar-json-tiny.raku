# A real module's grammar: JSON::Tiny::Grammar over a ~32 KB document (#9916).
#
# Every other grammar benchmark here runs a grammar written for the suite.
# ADR-0099 §2.3 showed why that is not enough: a hand-written grammar that
# omitted the `<sym>`-bodied proto variants flattered mutsu by 3.3x, and
# JSON::Tiny::Grammar has exactly that shape -- plus the scoped
# `(:ignoremark '"')` that was once quadratic (ADR-0099 §2.7). So this file parses
# with the module itself, loaded from the vendored copy under modules/ through
# `use lib`, and rakudo parses with the identical source.
#
# Grammar only, no actions: this is the regex engine's series, and ADR-0135
# Slice D (#10254) reads its grammar headroom off it.
#
# The document is generated and deterministic: 160 records with strings,
# escapes (`\n`, `\"`, `é`), integers, signed exponent floats, booleans,
# null, nested objects and arrays.
#
# WARM COST: two untimed parses, then a timed one printed as
# `bench-section-seconds:` -- see bench-regex-match.raku's header. Under
# scripts/bench-det.sh (BENCH_DET=1) the document shrinks to 16 records and
# is parsed once.
use lib $?FILE.IO.parent(2).add('modules/JSON-Tiny/lib').Str;
use JSON::Tiny::Grammar;

my $records = %*ENV<BENCH_DET> ?? 16 !! 160;
my $doc = '{"users": [' ~ (^$records).map(-> $i {
    '{"id": ' ~ $i
    ~ ', "name": "user ' ~ $i ~ '"'
    ~ ', "email": "u' ~ $i ~ '@example.com"'
    ~ ', "score": ' ~ ($i %% 3 ?? "-{$i}.5e{$i % 4}" !! $i * 17)
    ~ ', "active": ' ~ ($i %% 2 ?? 'true' !! 'false')
    ~ ', "manager": ' ~ ($i %% 5 ?? 'null' !! $i div 5)
    ~ ', "tags": ["t' ~ ($i % 7) ~ '", "g' ~ ($i % 3) ~ '"]'
    ~ ', "note": "line\nbreak é \"q' ~ $i ~ '\""'
    ~ ', "geo": {"lat": ' ~ ($i * 0.25) ~ ', "lon": -' ~ ($i * 0.5) ~ '}}'
}).join(', ') ~ ']}';

my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
JSON::Tiny::Grammar.parse($doc) for ^$warm;
my $t0 = now;
my $m = JSON::Tiny::Grammar.parse($doc);
say "bench-section-seconds: {now - $t0}";
die "parse failed" unless $m && $m.chars == $doc.chars;
say "grammar-json-tiny: ok (doc {$doc.chars} chars)";

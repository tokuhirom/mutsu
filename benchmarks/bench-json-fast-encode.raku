# JSON::Fast encode benchmark: `to-json` of a META6-shaped document, alone.
#
# bench-json-fast.raku times encode and decode together, and the two turned out
# to cost very differently (#8673, 2026-11-28): on a release build the encode
# took ~10x the decode, so the combined row could not say which half moved.
# This file isolates the encode -- `to-json`'s `jsonify` dispatch over each
# value, its `pretty-*` helpers, and `str-escape` on every key and string
# (short strings, so the per-call cost dominates, not the per-character scan).
# bench-json-fast-decode.raku is its twin for `from-json`.
#
# The document is the one bench-json-fast.raku uses. `use lib` names the
# vendored copy relative to this file so a bare `raku <file>` resolves it too
# (see bench-json-fast.raku's header for why that idiom is load-bearing).
use lib $?FILE.IO.parent(2).add('modules/JSON-Fast/lib').Str;
use JSON::Fast;

my %doc =
    name => 'Some::Distribution', version => '0.4.2', auth => 'zef:someone',
    license => 'Artistic-2.0', authors => ['A. Author', 'B. Author'],
    depends => (^25).map({ "Dep::Number$_" }).Array,
    provides => (^40).map({ "Mod::N$_" => "lib/Mod/N$_.rakumod" }).Hash,
    tags => <json parser fast>.Array,
    support => { source => 'https://example.invalid/repo.git' };

sub workload() {
    my int $chars = 0;
    for ^60 { $chars = $chars + to-json(%doc).chars }
    "json-fast-encode: 60 encodes, $chars chars";
}

# Warm timed section (docs/benchmarks.md): two untimed runs, the third timed.
# Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

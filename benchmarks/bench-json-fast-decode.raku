# JSON::Fast decode benchmark: `from-json` of a META6-shaped document, alone.
#
# The twin of bench-json-fast-encode.raku (see its header for why the two are
# split out of bench-json-fast.raku). This is the shape zef reads for every
# piece of distribution metadata: many SHORT strings, so the cost is per token
# -- `parse-thing` / `parse-obj` / `parse-string` calls and their `nqp::` ops
# -- rather than per character. bench-json-fast-spdx.raku is the large-document
# decode; this one is the small-document, call-heavy end.
use lib $?FILE.IO.parent(2).add('modules/JSON-Fast/lib').Str;
use JSON::Fast;

my %doc =
    name => 'Some::Distribution', version => '0.4.2', auth => 'zef:someone',
    license => 'Artistic-2.0', authors => ['A. Author', 'B. Author'],
    depends => (^25).map({ "Dep::Number$_" }).Array,
    provides => (^40).map({ "Mod::N$_" => "lib/Mod/N$_.rakumod" }).Hash,
    tags => <json parser fast>.Array,
    support => { source => 'https://example.invalid/repo.git' };
my $text = to-json(%doc);

sub workload() {
    my int $keys = 0;
    for ^300 { $keys = $keys + from-json($text)<provides>.elems }
    "json-fast-decode: 300 decodes of {$text.chars} chars, $keys provides keys";
}

# Warm timed section (docs/benchmarks.md): two untimed runs, the third timed.
# Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;

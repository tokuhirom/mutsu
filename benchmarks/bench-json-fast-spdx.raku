# JSON::Fast decode of a real-world-sized document, timed IN-PROCESS (#8673).
#
# This is the benchmark that decides #8673 ("from-json must beat rakudo"). It
# exists because `bench-json-fast` cannot answer that question: it times the
# whole script on a 2.4KB document, so rakudo's ~0.15s startup floor is larger
# than the JSON work and the row's ratio reads near 1 while the decode itself
# is ~60x slower.
#
# So this file times only the `from-json` call and prints it as
#
#     bench-section-seconds: <seconds>
#
# which scripts/bench-ci.sh picks up and records as an extra series,
# `bench-json-fast-spdx@section` (and `...@section+jit`), with raku's own
# in-process time in the raku column. THAT row's ratio is the #8673 number;
# the plain whole-script row next to it still includes both interpreters'
# startup and module loading.
#
# The document is a synthetic SPDX license list -- the file `License::SPDX`
# decodes on every `Test::META` `meta-ok()` call, which is how the ecosystem
# keeps hitting this (Lumberjack::Application, SixPM). Same shape as the real
# `licenses.json`: one top-level object, a `licenses` array of 727 flat
# records with nine string/bool/int/array fields each, pretty-printed with the
# real file's indentation, ~340KB. It is generated rather than vendored so the
# repository does not carry a third-party data file, and it is deterministic
# so every run parses byte-identical input.
#
# One decode per process, on purpose: that is what `License::SPDX` does, and
# it is where rakudo's JIT has had the least time to warm up. Repeating the
# decode would measure a steady state no real caller reaches.
#
# Deliberately ~3s on a release mutsu, far outside PERFORMANCE.md's 0.1-0.4s
# guideline: the gap widens with document size (rakudo gets faster per record
# as its JIT warms, mutsu does not), so a small document would under-report
# exactly the number this file exists for.
#
# Under scripts/bench-det.sh (BENCH_DET=1) the document shrinks to 100
# records: callgrind runs at ~90x native, and the instruction series only has
# to be stable across commits, not comparable with the wall-clock one.
use lib $?FILE.IO.parent(2).add('modules/JSON-Fast/lib').Str;
use JSON::Fast;

my $records = %*ENV<BENCH_DET> ?? 100 !! 727;

sub record(Int $i --> Str) {
    my $id = "Lic-{$i}-{$i % 7 ?? 'only' !! 'or-later'}";
    my $deprecated = ($i %% 11) ?? 'true' !! 'false';
    my $osi = ($i %% 3) ?? 'true' !! 'false';
    my @see = (^(1 + $i % 3)).map({
        qq|        "https://example.org/licenses/{$id}/text-{$_}.html"|
    });
    qq:to/END/.chomp;
        \{
          "reference": "https://spdx.org/licenses/{$id}.html",
          "isDeprecatedLicenseId": {$deprecated},
          "detailsUrl": "https://spdx.org/licenses/{$id}.json",
          "referenceNumber": {$i},
          "name": "Example License Number {$i}, a moderately long human readable title",
          "licenseId": "{$id}",
          "seeAlso": [
    {@see.join(",\n")}
          ],
          "isOsiApproved": {$osi}
        \}
    END
}

my $text = qq:to/END/;
\{
  "licenseListVersion": "3.29",
  "licenses": [
{(^$records).map(&record).join(",\n")}
  ],
  "releaseDate": "2026-07-01"
\}
END

my $t0 = now;
my $data = from-json($text);
my $elapsed = now - $t0;

die 'SPDX decode lost records' unless $data<licenses>.elems == $records;
die 'SPDX decode lost fields' unless $data<licenses>[5]<seeAlso>.elems == 3;

say "json-fast-spdx: {$text.chars} chars, $records records";
say sprintf('bench-section-seconds: %.6f', $elapsed);

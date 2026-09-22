# JSON::Fast benchmark: the vendored upstream module (a battery since #8288),
# over the two shapes whose costs turned out to be unrelated (#8289).
#
# 1. A META6-shaped document -- the shape zef walks for every metadata read,
#    and therefore the one that decides how slow `mzef` feels. It is many
#    SHORT strings, so its cost is per-token: sub calls and `nqp::` ops, each
#    of which mutsu pays roughly 3-6x rakudo for because it resolves calls by
#    building and comparing key strings. That is the open half of #8289 and
#    the reason this benchmark exists: the gap was ~63x rakudo on encode and
#    ~126x on decode when first measured, and nothing but a trend line will
#    show whether dispatch work moves it.
#
# 2. One LONG string. `str-escape` walks a string's NFD codepoints with
#    `nqp::elems`/`nqp::atpos_i`, which used to copy the whole element vector
#    per call and made encoding quadratic in string length. That is fixed, so
#    this half is a complexity guard: it is cheap while the scan stays linear
#    and grows without bound if the copy ever comes back.
#
# Sized so a release mutsu takes well under a second, because
# scripts/bench-det.sh re-runs every benchmark under callgrind at ~90x native.
#
# ----------------------------------------------------------------------------
# Resolving a bundled battery without `-I`.
#
# scripts/bench-ci.sh measures the reference interpreter with a bare
# `raku <file>` and passes no module search path, so a benchmark that just says
# `use JSON::Fast` records NA in the raku column and NA for the ratio -- which
# is the one number this benchmark exists to track. The line below fixes that
# for any interpreter, by naming the vendored copy relative to this FILE rather
# than to the working directory:
#
#     use lib $?FILE.IO.parent(2).add('modules/<Dist>/lib').Str;
#
# It is the idiom for any benchmark that needs a battery (bench-yaml-parse and
# bench-yaml-parse-big still take the `-I`-less NA and could adopt it). Two
# properties are load-bearing:
#
# - It names ONE distribution, not `modules/*/lib`. Globbing every battery onto
#   the search path would add ~36 directories of unrelated module-resolution
#   work to the thing being timed, and makes rakudo emit a deprecation warning
#   for JSON-Tiny's `.pm` files on top.
# - It changes nothing for mutsu, whose bundled batteries ARE `modules/<Dist>/
#   lib` (`Interpreter::bundled_lib_paths`), so `use lib` merely names the file
#   mutsu would have loaded anyway. The mutsu series stays comparable across
#   this change; only the raku column goes from NA to a real measurement.
#
# Read that ratio for what it is: WHOLE-SCRIPT time, including rakudo's startup
# floor, which is ~0.15s on the bench runner and larger than the JSON work it
# does here. That is true of every row in this suite -- the raku column across
# the whole history sits in 0.14-0.30s -- and is the documented design, since
# the ratio exists to normalize runner speed rather than to isolate one
# operation. So this row's ratio will read near 1, and it is NOT the ~63x/~126x
# of #8289, which was measured on the encode/decode calls alone with startup
# excluded. What it does do is move when dispatch cost moves, which is the
# point; for the isolated per-operation figure, time the calls directly --
# which is what bench-json-fast-spdx does, as its `@section` rows (#8673).
# ----------------------------------------------------------------------------
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
for ^6 { to-json(%doc) }

my $round-trip = from-json($text);
die 'META6 round-trip lost keys' unless $round-trip.elems == %doc.elems;
for ^3 { from-json($text) }

# The complexity guard. 8,000 characters cost 0.305s per encode while the
# scan was quadratic and 0.047s once it was linear, so a regression here is
# loud rather than marginal.
my %long = blob => ('x' x 8000);
my $long-text = to-json(%long);
die 'long-string round-trip lost the payload'
    unless from-json($long-text)<blob>.chars == 8000;
for ^2 { to-json(%long) }

say "json-fast: {$text.chars} byte doc, {$long-text.chars} byte long-string doc";

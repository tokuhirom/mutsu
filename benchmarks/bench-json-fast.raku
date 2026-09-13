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
# The raku column and the ratio record NA for this row, as they do for every
# battery-using benchmark (bench-yaml-parse and the grammar ones): scripts/
# bench-ci.sh measures the reference with a bare `raku <file>`, and a stock
# rakudo has no JSON::Fast to find. The mutsu absolute series and the
# deterministic instruction counts are the signal here. To get the cross-
# interpreter number by hand, point rakudo at the vendored copy:
#
#     raku -I modules/JSON-Fast/lib benchmarks/bench-json-fast.raku
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

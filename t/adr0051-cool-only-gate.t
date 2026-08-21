use v6;
use Test;

# ADR-0051 (docs/adr/0051-type-ancestry-has-one-oracle-and-an-unresolved-
# method-throws.md) phases P3/P4: a plain (Any-derived, non-Cool) class no
# longer answers a Cool-only builtin method by stringifying the receiver;
# it dies with X::Method::NotFound, matching real Rakudo. Meanwhile every
# genuine Cool-derived / own-method call (P1's ancestry fix + P3's row
# additions) still resolves correctly.

plan 12;

# The reported symptom: a plain class must NOT answer `.uc`.
class G {}
throws-like { G.new.uc }, Exception, message => /uc/,
    'G.new.uc dies (plain class has no Cool ancestry)';

my $caught;
try {
    G.new.uc;
    CATCH { default { $caught = $_ } }
}
isa-ok $caught, X::Method::NotFound, 'the exception is a real X::Method::NotFound';

# Genuine Cool-derived types (P1 ancestry fix) still resolve their Cool
# methods correctly through the new gate.
my $i = now;
lives-ok { $i.abs }, 'Instant.abs still resolves (Instant is Cool)';

lives-ok { "/tmp".IO.chars }, 'IO::Path.chars still resolves (IO::Path is Cool)';

# Own-method rows (P3): already dispatched correctly, now also visible to
# .^can / e2_native_method_exists.
my $dt = DateTime.now;
isa-ok $dt.Date, Date, 'DateTime.Date still resolves';

isa-ok $i.DateTime, DateTime, 'Instant.DateTime still resolves';

isa-ok Date.new(2020, 3, 5).IO, IO::Path, 'Date.IO still resolves';

# .^can agrees with dispatch now (was 0 in mutsu, 1 in raku, before P3).
ok Instant.^can("DateTime").elems, 'Instant.^can("DateTime") is nonzero';

ok Date.^can("IO").elems, 'Date.^can("IO") is nonzero';

# CI regression (2026-08-21, fixed forward on the same PR): a cool_only name
# shared with a genuine, receiver-aware by-name dispatcher arm (Supply's
# own `.comb`/`.words` combinators, which explicitly check for a Supply
# receiver and have no entry in the E2 native-method-row catalog at all)
# must NOT be blocked just because that E2 lookup misses. Blanket-gating
# `shadows_builtin` broke this; the fix keeps the P4 gate only at the two
# genuinely receiver-class-blind by-name arms (`.IO`, `.subst`) plus the
# arity-cascade gate sites.
lives-ok { Supply.from-list(<a b c>).comb.tap(-> $v { }) },
    'Supply.comb still resolves (own combinator, not a Cool coercion)';

lives-ok { Supply.from-list(<a b c>).words.tap(-> $v { }) },
    'Supply.words still resolves (own combinator, not a Cool coercion)';

# CI regression (2026-08-21): StrDistance genuinely inherits Cool in real
# Rakudo but was missing from builtin_type_catalog (same shape as P1's
# Instant/Duration/IO::Path rows, just not one of the types that
# investigation's `make test` run happened to surface).
my $sd = StrDistance.new(:before<a>, :after<b>);
lives-ok { $sd.Rat }, 'StrDistance.Rat resolves (StrDistance is Cool)';

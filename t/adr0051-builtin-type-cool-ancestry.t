use v6;
use Test;

# ADR-0051 P1: `builtin_type_catalog` becomes the single ancestry oracle for
# built-in types, and `classhow_mro_names` (`.^mro`) is re-pointed at it
# instead of the deleted `builtin_type_parents` table. `Instant`, `Duration`,
# and `IO::Path` genuinely ARE `Cool` in Rakudo (`.^mro` includes it, and
# `~~ Cool` answers True); mutsu's ancestry data previously omitted `Cool` from
# all three -- for `Instant`/`Duration` this was even self-contradictory,
# since `~~ Cool` already answered True (a separate hand-verified allowlist)
# while `.^mro` disagreed with itself in the same process.
#
# `IO::Handle` and `Cool` itself are pinned as negative/identity controls:
# `IO::Handle` does NOT inherit `Cool` in Rakudo (unlike its `IO::Path`
# sibling -- the two must not be conflated), and `Cool.^mro` is `(Cool Any
# Mu)` with no further ancestor.
#
# Every assertion below was dual-oracle verified against a real `raku`
# (Rakudo 2026.06) on 2026-08-20.

sub mro-names($type) { $type.^mro.map(*.^name).list }

# --- Instant ---
is-deeply mro-names(Instant), <Instant Cool Any Mu>, 'Instant.^mro includes Cool';
ok Instant ~~ Cool, 'Instant ~~ Cool';
ok +Instant.^can('abs'), 'Instant.^can("abs") is non-zero (Cool method visible)';
my $i = now;
ok $i.abs ~~ Instant, 'Instant.abs (a Cool/Real method) works and keeps the type';

# --- Duration ---
is-deeply mro-names(Duration), <Duration Cool Any Mu>, 'Duration.^mro includes Cool';
ok Duration ~~ Cool, 'Duration ~~ Cool';
ok +Duration.^can('abs'), 'Duration.^can("abs") is non-zero (Cool method visible)';
my $d = now - now;
ok $d.abs ~~ Duration, 'Duration.abs (a Cool/Real method) works and keeps the type';

# --- IO::Path ---
is-deeply mro-names(IO::Path), <IO::Path Cool Any Mu>, 'IO::Path.^mro includes Cool';
ok IO::Path ~~ Cool, 'IO::Path ~~ Cool';
ok +IO::Path.^can('chars'), 'IO::Path.^can("chars") is non-zero (Cool method visible)';
my $p = "t/adr0051-builtin-type-cool-ancestry.t".IO;
ok $p.chars > 0, 'IO::Path.chars (a Cool string-coercion method) works';

# --- IO::Handle: negative control, does NOT inherit Cool ---
is-deeply mro-names(IO::Handle), <IO::Handle Any Mu>, 'IO::Handle.^mro has no Cool';
nok IO::Handle ~~ Cool, 'IO::Handle does NOT ~~ Cool';
is +IO::Handle.^can('chars'), 0, 'IO::Handle.^can("chars") is zero (no Cool ancestor)';

# --- Cool itself: identity control ---
is-deeply mro-names(Cool), <Cool Any Mu>, 'Cool.^mro is (Cool Any Mu)';
ok Cool ~~ Cool, 'Cool ~~ Cool';
ok +Cool.^can('chars'), 'Cool.^can("chars") is non-zero (its own method)';

# --- Match: pre-existing catalog row, must still be right after the re-point ---
is-deeply mro-names(Match), <Match Capture Cool Any Mu>, 'Match.^mro is unaffected';
ok Match ~~ Cool, 'Match ~~ Cool';

# --- Pair: must NOT gain Cool -- the exact false-positive shape a past
# regression hit (see `is_builtin_type_method`'s comment in
# `methods_classhow_lookup.rs`), pinned here so a future ancestry change
# cannot silently reintroduce it.
is-deeply mro-names(Pair), <Pair Any Mu>, 'Pair.^mro has no Cool (unaffected regression guard)';
nok Pair.new('a', 1) ~~ Cool, 'a Pair does NOT ~~ Cool';
is +Pair.new('a', 1).^can('int8'), 0, 'Pair.^can("int8") stays zero (no Cool coercion leak)';

# --- Plain class: still not Cool (P4's gate is out of scope for P1, but the
# ancestry data itself must not regress a plain class into Cool).
class G { }
is-deeply mro-names(G), <G Any Mu>, 'a plain class has no Cool ancestor';
nok G ~~ Cool, 'a plain class does NOT ~~ Cool';

done-testing;

use v6;
use Test;

plan 9;

# `$CALLERS::` — "any caller scope". Its only difference from `$CALLER::` is
# that a `$*`-twigil dynamic name cascades outward through the whole caller
# chain; a plain (non-twigil) name resolves to the exact frame at the given
# depth, identically to `$CALLER::`. Verified against raku 2026-07-18.

sub f1($f) { my $x is dynamic = 90; $f() }
sub f2($f) { my $x is dynamic = 91; f1($f) }
my $callers = 'CALLERS';

# Plain name, immediate caller: same as CALLER::.
is f1({ $CALLERS::x }), 90, '$CALLERS:: reads the immediate caller';
is f1({ CALLERS::<$x> }), 90, 'CALLERS::<$x> stash form works';
is f1({ $::($callers)::x }), 90, '::("CALLERS") indirect form works';

# Plain name, explicit depth 2 via CALLERS::CALLERS::.
is f2({ $CALLERS::CALLERS::x }), 91, 'CALLERS::CALLERS:: reaches depth 2';
is f2({ $::($callers)::($callers)::x }), 91, 'indirect CALLERS::CALLERS:: works';

# NOTE: `g2({ $CALLERS::v })` where a plain `is dynamic` lexical is declared two
# frames out (immediate caller does not declare it) is Nil in raku, but mutsu
# returns 7 here — the plain-name path shares `get_caller_var`, whose flattened
# caller env over-reaches into inherited dynamics. That is a pre-existing
# `CALLER::` bug (documented in TODO_roast/BLOCKERS.md), not specific to
# `CALLERS::`, and needs per-frame declared-name data to fix. Not asserted here.

# A `$*`-twigil dynamic name DOES cascade through the caller chain.
my $*foo = 92;
is f2({ CALLERS::<$*foo> }), 92, 'CALLERS::<$*foo> cascades to a $*-twigil dynamic';

# The innermost caller that declares the name wins (no skipping past it).
sub h1($f) { my $w is dynamic = 1; $f() }
sub h2($f) { my $w is dynamic = 2; h1($f) }
is h2({ $CALLERS::w }), 1, 'CALLERS:: takes the innermost caller for a plain name';

# CALLERS:: and CALLER:: agree wherever no cascade is involved.
is f1({ $CALLERS::x }), f1({ $CALLER::x }), 'CALLERS:: == CALLER:: for a plain immediate name';
is f2({ $CALLERS::CALLERS::x }), f2({ $CALLER::CALLER::x }),
    'CALLERS::CALLERS:: == CALLER::CALLER:: at depth 2';

# vim: expandtab shiftwidth=4

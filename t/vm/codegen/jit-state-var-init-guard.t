use v6;
use Test;

# JIT Tier A (ADR-0004 §2.3): `StateVarInitGuard`/`StateVarInit` (the
# once-only initialization check/store a `state` declaration compiles to)
# used to be outside the Tier A opcode set, so any routine body containing a
# `state` declaration permanently bailed out of JIT compilation and ran
# interpreted forever -- roast/S04-declarations/state.t's "intensive use of
# state variable in inline-friendly sub" subtest was 12x slower than raku
# because of exactly this bailout (todo/tickets/state-var-init-guard-jit-
# bailout-blocks-hot-loop.md). `StateVarInitGuard` now gets its own
# conditional-branch codegen arm (mirroring JumpIfFalse/JumpIfTrue/
# JumpIfNotNil) and `StateVarInit` is on the generic step-shim whitelist, so
# a hot `state`-using routine JIT-compiles cleanly instead of bailing.
#
# Every sub here is called >= 300 times (comfortably above the default
# MUTSU_JIT_THRESHOLD=100 call-count hotness bar) so a default MUTSU_JIT=on
# run actually exercises the native body, not just the interpreter.

plan 5;

sub counter() { state $n = 0; $n++; $n }
my $last = 0;
for ^500 { $last = counter() }
is $last, 500, 'state scalar counter across 500 JIT-hot calls';

sub accumulator($x) { state $total = 0; $total += $x; $total }
my $sum = 0;
for ^400 { $sum = accumulator(2) }
is $sum, 800, 'state scalar accumulator across 400 JIT-hot calls';

sub grower() { state @seen = (); @seen.push('x'); @seen.elems }
my $len = 0;
for ^350 { $len = grower() }
is $len, 350, 'state array grows exactly once per call across 350 JIT-hot calls';

sub topic_init() { $ = 42 }
my $topic_last;
for ^500 { $topic_last = topic_init() }
is $topic_last, 42, 'bare topic-assignment state (roast state.t shape) across 500 JIT-hot calls';

# Non-hot (below the default threshold) sanity check: same shape, few calls,
# so this exercises the plain interpreter path unconditionally.
sub cold_counter() { state $n = 0; $n++; $n }
my @vals = (cold_counter() for ^5);
is-deeply @vals, [1, 2, 3, 4, 5], 'cold (non-JIT-triggering) state counter still correct';

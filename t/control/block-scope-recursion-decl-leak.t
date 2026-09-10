use v6;
use Test;

# A routine that recurses into itself from inside a bare `{ ... }` block (or a
# pointy `while ... -> $x { }` loop, which compiles to a Block wrapping a While)
# must not have the callee's routine-level `my $x` declaration register in the
# caller's active `BlockScope` frame. Previously the callee's `my $r` was tracked
# as block-local in the caller's block, so the caller's block exit reverted `$r`
# to its pre-block value, dropping the caller's own accumulation across the call.
# Regression for the zef `find-prereq-candidates` for-loop chain.

plan 8;

# --- bare block, mutate before the recursive call ---
sub a($n) { my $r = 0; { $r = 10; a($n - 1) if $n > 0 }; $r }
is a(1), 10, 'bare block, mutate-before, depth 1';
is a(2), 10, 'bare block, mutate-before, depth 2';

# --- bare block, mutate after the recursive call ---
sub c($n) { my $r = 0; { c($n - 1) if $n > 0; $r += 10 }; $r }
is c(1), 10, 'bare block, mutate-after, depth 1';

# --- pointy while (compiles to Block([While ...])) ---
sub b($n) { my $r = 0; my @w = (1,); while @w.splice -> @x { b($n - 1) if $n > 0; $r += 10 }; $r }
is b(1), 10, 'pointy while, recursion';

# --- bare block wrapping a while, accumulator ---
sub d($n) { my $r = 0; my @w = (1,); { while @w.splice { $r += 10; d($n - 1) if $n > 0 } }; $r }
is d(1), 10, 'bare block around while, recursion';

# --- non-light call paths must be fixed too ---
sub s2(*@a) { my $n = @a[0]; my $r = 0; { $r = 10; s2($n - 1) if $n > 0 }; $r }
is s2(1), 10, 'slurpy-param sub (general call path)';

sub nm(:$n) { my $r = 0; { $r = 10; nm(n => $n - 1) if $n > 0 }; $r }
is nm(n => 1), 10, 'named-param sub (general call path)';

# --- deeper recursion accumulates correctly (each frame owns its $r) ---
sub e($n) { my $r = 0; { e($n - 1) if $n > 0; $r += 5 }; $r }
is e(3), 5, 'each recursion frame keeps its own accumulator';

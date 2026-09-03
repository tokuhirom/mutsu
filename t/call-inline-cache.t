use v6;
use Test;

# ADR-0066: a call site caches the routine it resolved to. Every mechanism that
# can change what a name resolves to must retire that cache. These are the
# adversarial shapes named in the ADR's validation plan.

plan 13;

# 1. A plain repeated call site keeps resolving to the same routine.
sub twice($n) { $n * 2 }
sub call-twice-many() {
    my $sum = 0;
    $sum += twice($_) for 1 .. 5;
    $sum;
}
is call-twice-many(), 30, 'repeated call site resolves consistently';

# 2. The same bare name in two packages is two different routines, even though
#    both call sites are compiled from the same source text shape.
module PkgA {
    our sub which() { 'A' }
    our sub probe() { which() }
}
module PkgB {
    our sub which() { 'B' }
    our sub probe() { which() }
}
is PkgA::probe(), 'A', 'package A call site resolves to its own sub';
is PkgB::probe(), 'B', 'package B call site resolves to its own sub';
is PkgA::probe(), 'A', 'package A still resolves to its own sub after B ran';

# 3. `wrap` replaces what a name dispatches to, mid-run, after the site is hot.
sub wrapped($n) { $n + 1 }
sub call-wrapped($n) { wrapped($n) }
is call-wrapped(1), 2, 'pre-wrap dispatch';
my $handle = &wrapped.wrap(-> $n { callsame() * 10 });
is call-wrapped(1), 20, 'wrap is visible at an already-executed call site';
&wrapped.unwrap($handle);
is call-wrapped(1), 2, 'unwrap restores the original at the same call site';

# 4. The same routine name called from two different compilation units: the
#    mainline's table and the EVAL's are different tables, and a call site may
#    not answer for one with what it resolved in the other.
sub shared($n) { $n * 3 }
sub call-shared($n) { shared($n) }
is call-shared(2), 6, 'mainline unit resolves its own sub';
is EVAL('shared(3)'), 9, 'the same name called from an EVAL unit';
is call-shared(2), 6, 'the mainline call site is unaffected by the EVAL unit';

# 5. A block-local routine shadows an outer same-named one for calls inside the
#    block, without disturbing the outer call site that already ran.
sub picked() { 'outer' }
sub pick() { picked() }
is pick(), 'outer', 'outer site before the shadow exists';
{
    sub picked() { 'inner' }
    is picked(), 'inner', 'the block-local declaration shadows inside the block';
}
is pick(), 'outer', 'the outer call site still reaches the outer routine';

done-testing;

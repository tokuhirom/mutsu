# A sub called from several packages keeps one call-cache entry per package

The on-the-fly call cache answers "what does this bare routine name mean
here" and must not leak an answer from one package into another, so each
entry recorded the callsite package and a call from any other package missed.
The cache was keyed by the name alone, though: a miss resolved the name again
and overwrote the entry, so a helper called alternately from two packages
evicted its own entry on every call and paid the whole resolution walk each
time.

The callsite package (and, for a module's private routine, its unit) is now
part of the key. Monad's type helpers are called from `Monad::Maybe`'s and
from a subtype role's methods in turn; Monad `t/14_concurrent_types.t` drops
from ~15.1M to ~13.1M instructions per chain pair, and the full file from
181 s to 154 s on a 4-core box (#17331).

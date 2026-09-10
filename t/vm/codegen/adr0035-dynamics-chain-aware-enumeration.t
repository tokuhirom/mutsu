use Test;

# ADR-0035 (docs/adr/0035-method-calls-observe-caller-frames.md) Mechanism 1:
# `dynamic_pseudo_stash_entries` (backing `PROCESS::`/`DYNAMIC::`) walked each
# frame env with `Env::iter()`, which only sees the TOP overlay tier, while
# `Env::get()` walks the whole parent chain (with tombstone suppression). Any
# frame that runs under a scoped overlay without pushing its own caller-env
# entry is therefore invisible-through: dynamics living in its parent tiers
# were unreachable from the `PROCESS::`/`DYNAMIC::` stash walk even though a
# direct `$*x`-style read of the SAME name would find them.
#
# Four shapes, all verified against raku directly (all print 42 there):
#   - a flat method body (every compiled method runs under a scoped overlay
#     and never pushed a caller-env entry -- broken for ALL methods)
#   - a method body with an inner closure (worked by accident already, since
#     both method executors skip the overlay entirely when the body has a
#     closure -- must stay working)
#   - a sub WITH a positional parameter (takes the positional-light path:
#     scoped overlay, no caller-env push -- a latent sub-side gap)
#   - a frameless overlay intermediate sitting between the writer and a
#     reader sub that works fine when called directly (another latent gap)
plan 4;

PROCESS::<$X> = 42;

class C1 { method reader() { PROCESS::<$X> } }
is C1.new.reader(), 42, 'PROCESS:: is visible from a flat method body';

class C2 { method reader() { my $f = { 1 }; PROCESS::<$X> } }
is C2.new.reader(), 42, 'PROCESS:: stays visible from a closure-bearing method body';

sub reader-with-positional($n) { PROCESS::<$X> }
is reader-with-positional(1), 42, 'PROCESS:: is visible from a sub with a positional param';

sub reader-plain() { PROCESS::<$X> }
sub mid($n) { reader-plain() }
is mid(1), 42, 'PROCESS:: is visible through a frameless overlay intermediate sub';

use v6;
use Test;

plan 12;

# `try_native_method_raw` asks, of every instance receiver, whether it must
# decline the pure-native fast path in favour of the interpreter's Numeric
# bridge: does the receiver match `Real` or `Numeric`, or does its class provide
# a `Bridge` method? That probe is now MEMOIZED per receiver class, keyed on the
# registry write generation (#7712) — two full type-graph walks per instance
# method call were 52% of a `Buf.push` loop.
#
# The memo is only correct while two things hold, and this file pins both:
#
#   1. the answer is a property of the CLASS, so a class that does `Real` (or
#      reaches it through a role, or through inheritance) must keep bridging on
#      every call, not just the first;
#   2. anything that could change the answer goes through a registry write, and
#      a registry write invalidates the memo — so a `Bridge` method (or a whole
#      numeric class) that only exists *after* the class was already probed must
#      still be seen.

# --- 1. a class that does Real bridges, repeatedly -------------------------

class Temp does Real {
    has $.deg;
    method Bridge { $!deg.Num }
}

my $t = Temp.new(deg => 3);
is $t + 1, 4, 'Real-role instance bridges for arithmetic';
is $t.Int, 3, 'Real-role instance bridges for .Int';
is $t.Int, 3, '... and again, off the memoized probe';
is $t.abs, 3, '... and for a Cool-only numeric method';
ok $t < 4, 'Real-role instance bridges for comparison';

# A SECOND instance of the same class takes the memo hit rather than the walk.
my $t2 = Temp.new(deg => 7);
is $t2.Int, 7, 'a second instance of the same class bridges identically';

# --- 2. the answer reaches Real through a role and through inheritance -----

role Scaled does Real {
    has $.raw;
    method Bridge { $!raw.Num * 2 }
}
class Doubled does Scaled { }
is Doubled.new(raw => 4).Int, 8, 'Real reached through a composed role bridges';

class Warmer is Temp { }
is Warmer.new(deg => 9).Int, 9, 'Real reached through a parent class bridges';

# A plain class next to them is NOT numeric, and stays that way — the memo is
# per class, so a numeric neighbour must not leak into it.
class Plain { has $.n }
my $p = Plain.new(n => 5);
is $p.n, 5, 'a plain class keeps working as a plain class';
nok (try $p.Int).defined, 'a plain class does not acquire a Bridge from its neighbours';

# --- 3. a registry write after the class was probed invalidates the memo ---

# `Late` is probed here (no Bridge, not Real) and the answer memoized...
class Late { has $.n }
my $l = Late.new(n => 5);
is $l.n, 5, 'the pre-augment probe runs and is memoized';

# ... and then `augment` adds the `Bridge` that flips the answer. `augment`
# writes the registry, which bumps the write generation the memo is keyed on, so
# the stale `false` must not survive. EVAL keeps the augment at RUNTIME, after
# the probe above has already cached.
EVAL 'use MONKEY-TYPING; augment class Late { method Bridge { self.n.Num } }';
is $l.Int, 5, 'a Bridge added after the class was probed invalidates the memo';

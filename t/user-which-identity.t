use Test;

# A class may override `WHICH` to give its instances value semantics. That
# answer must decide `===`, `eqv`, Set/Bag/Mix element keys, object-hash keys
# and set membership -- not the per-object identity mutsu falls back to for a
# class that does not override `WHICH`.
#
# All expectations below were measured against rakudo v2026.07.

plan 42;

class WithWhich {
    has $.a;
    method WHICH { ValueObjAt.new: "WithWhich|$!a.WHICH()" }
}
class Plain {
    has $.a;
}
sub w($a) { WithWhich.new(:$a) }
sub p($a) { Plain.new(:$a) }

# --- the element's own identity -------------------------------------------
is w(5).WHICH.Str, 'WithWhich|Int|5', 'user WHICH is honoured for the object';
ok w(5) === w(5), 'two objects with the same user WHICH are ===';
nok w(5) === w(6), 'a different user WHICH is not ===';
ok w(5) eqv w(5), 'eqv follows the user WHICH too';

# --- Set / Bag / Mix element keys -----------------------------------------
is Set(w(5), w(5)).elems, 1, 'a Set collapses two objects with the same WHICH';
is Set(w(5), w(6)).elems, 2, 'a Set keeps objects with different WHICHes apart';
is Bag(w(5), w(5)).total, 2, 'a Bag counts both occurrences under one key';
is Bag(w(5), w(5)).elems, 1, 'a Bag holds one key for them';
is Mix(w(5), w(5)).elems, 1, 'a Mix holds one key for them';

# --- the container's own .WHICH -------------------------------------------
ok Set(w(5)).WHICH eq Set(w(5)).WHICH, 'Set.WHICH is built from the element WHICHes';
ok Bag(w(5)).WHICH eq Bag(w(5)).WHICH, 'Bag.WHICH is built from the element WHICHes';
ok Mix(w(5)).WHICH eq Mix(w(5)).WHICH, 'Mix.WHICH is built from the element WHICHes';
nok Set(w(5)).WHICH eq Set(w(6)).WHICH, 'different elements give a different Set.WHICH';
ok Set(w(5)) === Set(w(5)), 'Set === follows the element WHICHes';
ok Set(w(5)) eqv Set(w(5)), 'Set eqv follows the element WHICHes';
ok Set(w(5), w(6)).WHICH eq Set(w(6), w(5)).WHICH, 'Set.WHICH is order-independent';

# --- .Set / .Bag coercion from a list -------------------------------------
ok (w(5), w(6)).Set.WHICH eq (w(5), w(6)).Set.WHICH, '.Set coercion keys by user WHICH';
ok (w(5), w(6)).Bag.WHICH eq (w(5), w(6)).Bag.WHICH, '.Bag coercion keys by user WHICH';
is (w(5), w(5)).Set.elems, 1, '.Set coercion collapses equal WHICHes';

# --- set operators ---------------------------------------------------------
my $s = Set(w(5), w(6));
my $t = Set(w(6), w(7));
is ($s (|) $t).elems, 3, 'union merges the shared element';
is ($s (&) $t).elems, 1, 'intersection finds the shared element';
is ($s (-) $t).elems, 1, 'difference removes the shared element';
ok w(5) (elem) $s, 'a fresh object with the same WHICH is (elem) of the Set';
nok w(9) (elem) $s, 'an object with a different WHICH is not (elem)';

# --- object hash keys ------------------------------------------------------
my %h{Any};
%h{w(5)} = 'first';
is %h{w(5)}, 'first', 'an object hash finds the key by user WHICH';
%h{w(5)} = 'second';
is %h.elems, 1, 'storing under the same WHICH reuses the entry';
is %h{w(5)}, 'second', 'and overwrites its value';
ok %h{w(5)}:exists, ':exists finds the key by user WHICH';
nok %h{w(9)}:exists, ':exists misses a different WHICH';
%h{w(5)}:delete;
is %h.elems, 0, ':delete removes the entry found by user WHICH';

# --- any stringifiable WHICH gives value semantics -------------------------
# Rakudo compares `.WHICH` with `eq`, so a plain Str works exactly like an
# ObjAt or ValueObjAt -- there is no ValueObjAt-only rule.
class StrWhich   { has $.a; method WHICH { 'StrWhich|' ~ $!a } }
class ObjAtWhich { has $.a; method WHICH { ObjAt.new('ObjAtWhich|' ~ $!a) } }
ok StrWhich.new(a=>1) === StrWhich.new(a=>1), 'a plain Str WHICH gives value semantics';
is Set(StrWhich.new(a=>1), StrWhich.new(a=>1)).elems, 1, 'and collapses in a Set';
ok ObjAtWhich.new(a=>1) === ObjAtWhich.new(a=>1), 'an ObjAt WHICH gives value semantics';
is Set(ObjAtWhich.new(a=>1), ObjAtWhich.new(a=>1)).elems, 1, 'and collapses in a Set';

# --- control: a class WITHOUT a user WHICH keeps object identity ----------
nok p(5) === p(5), 'two distinct plain objects are not ===';
is Set(p(5), p(5)).elems, 2, 'a Set keeps two distinct plain objects apart';
my $one = p(5);
ok $one === $one, 'a plain object is === to itself';
is Set($one, $one).elems, 1, 'and collapses to one Set element';
ok Set($one).WHICH eq Set($one).WHICH, 'a Set over the same object has a stable WHICH';
my %g{Any};
%g{$one} = 'x';
is %g{$one}, 'x', 'an object hash still keys a plain object by identity';
is %g{p(5)}, Any, 'and a different plain object is a different key';
is %g.elems, 1, 'so the hash still holds one entry';

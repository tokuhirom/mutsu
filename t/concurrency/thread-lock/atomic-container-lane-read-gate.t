use Test;

# Regression pin for the `@`/`%` twin of t/atomic-read-gate.t: every array or
# hash variable READ used to build a `format!("__mutsu_atomic_arr::{name}")`
# String and walk the cross-thread store for it, to see whether a concurrent
# push / element assign had published an authoritative copy under that lane.
# That probe is now gated on a monotonic "an atomic container lane entry has
# been created" flag (runtime/shared_store.rs), so a program that never runs a
# concurrent container op pays nothing per read.
#
# The gate must not change observable semantics: a lane can only be READ after
# it has been WRITTEN, and the write arms the flag.

plan 12;

# --- Gate OFF: ordinary container reads are unaffected. -----------------------
my @a = 1, 2, 3;
is @a.elems, 3, 'ordinary array read unaffected by the gate';
is @a[1], 2, 'ordinary array element read unaffected';

my %h = a => 1, b => 2;
is %h.elems, 2, 'ordinary hash read unaffected by the gate';
is %h<b>, 2, 'ordinary hash element read unaffected';

# Reads through a closure's captured container (the hot free-variable path).
my $sum = -> { [+] @a };
is $sum(), 6, 'closure read of a captured array unaffected';

# A method reading its own `@!`/`%!` attributes — the bench-ctor shape.
class Holder {
    has @.items;
    has %.meta;
    method total() { @!items.elems + %!meta.elems }
}
is Holder.new(items => [1, 2], meta => { x => 1 }).total, 3,
    'attribute container reads unaffected by the gate';

# --- Arm the gate: concurrent pushes go through the atomic array lane. --------
my @shared;
my $lock = Lock.new;
await (^4).map: {
    start {
        for ^25 {
            $lock.protect: { @shared.push(1) }
        }
    }
};
is @shared.elems, 100, 'concurrent pushes all landed in the shared array';
is ([+] @shared), 100, 'shared array contents are intact';

# --- Gate now ON: ordinary reads must still be correct. ----------------------
is @a.elems, 3, 'ordinary array read still correct after the gate is armed';
is %h<a>, 1, 'ordinary hash read still correct after the gate is armed';
is $sum(), 6, 'closure array read still correct after the gate is armed';

# A concurrently-written hash reads back through the same lane.
my %shared;
my $hlock = Lock.new;
await (^4).map: -> $i {
    start {
        $hlock.protect: { %shared{"k$i"} = $i }
    }
};
is %shared.elems, 4, 'concurrent hash element writes all visible';

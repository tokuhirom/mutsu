use v6;
use Test;

# Array/hash twin of `t/thread-callee-param-does-not-clobber-caller.t`.
#
# The cross-thread shared store is keyed by BARE NAME, so it cannot represent
# two concurrently-live bindings of one name. `shared_vars_active` never goes
# back to false, so once ANY thread has run, every plain lexical container
# funnelled into that store aliased every other binding of the same name: a
# routine's own `my @a` mutated from a nested sub wrote through to the caller's
# `@a`. (`Cro.compose`'s `my @components` became `Cro::ConnectionManager`'s
# `:@components` parameter, so every multi-server Cro::HTTP test died with
# "Components controlled by a connection manager must compose to form a
# transform or a sink".)
#
# A `my @a` / `my %h` re-declared under the active shared lane is now masked out
# of the name lane in every direction (writes, atomic-store routing, reads), and
# the mask is lifted at the next spawn so genuinely shared containers keep
# working.

plan 14;

await start { 1 };   # arm the shared lane; it never disarms

# --- a routine-local container mutated from a NESTED SUB stays private -------

sub push-from-nested-sub() {
    my @arr;
    sub bump() { @arr.push('x') }
    bump();
    @arr.join(",")
}

sub listop-push-from-nested-sub() {
    my @arr;
    sub bump2() { push @arr, 'x' }
    bump2();
    @arr.join(",")
}

sub hash-from-nested-sub() {
    my %h;
    sub bump3() { %h<k> = 'v' }
    bump3();
    %h.keys.join(",")
}

my @arr = <SENTINEL>;
my %h = :outer(1);

is push-from-nested-sub(), "x", "callee's own \@arr got the push";
is @arr.join(","), "SENTINEL", "caller's \@arr is untouched by a nested-sub .push";

is listop-push-from-nested-sub(), "x", "callee's own \@arr got the listop push";
is @arr.join(","), "SENTINEL", "caller's \@arr is untouched by a nested-sub listop push";

is hash-from-nested-sub(), "k", "callee's own %h got the element";
is %h.keys.join(","), "outer", "caller's %h is untouched by a nested-sub element write";

# Each call gets a fresh binding, not an accumulating shared one.
is push-from-nested-sub(), "x", "a second call starts from an empty \@arr again";

# --- a worker thread's own `my @a` is not the parent's -----------------------

my @shadowed = <PARENT>;
is (await start { my @shadowed; @shadowed.push('w'); @shadowed.join(",") }), "w",
    "a worker's own my \@shadowed starts empty";
is @shadowed.join(","), "PARENT", "the parent's \@shadowed is untouched";

# --- genuinely shared containers still work ----------------------------------

my @shared;
await Promise.allof( (^4).map: -> $i { start { @shared.push($i) for ^25 } } );
is @shared.elems, 100, "concurrent pushes to a genuinely shared \@shared all land";

my @late;                       # declared AFTER a thread has run
await start { @late.push($_) for ^10 };
is @late.elems, 10, "a lexical declared under the shared lane is still shared with a later spawn";

my @slots = 0 xx 4;
await Promise.allof( (^4).map: -> $i { start { @slots[$i] = $i * 2 } } );
is @slots.join(","), "0,2,4,6", "concurrent element assignment still writes through";

my %shared;
await Promise.allof( (^8).map: -> $i { start { %shared{"k$i"} = $i } } );
is %shared.elems, 8, "concurrent hash element assignment still writes through";

# --- a worker-local container stays coherent across DIFFERENT mutators -------
# Denying the name lane must not lose updates: `push` then `append` on one
# thread-local array both have to land (the shape that regressed
# t/hyper-array-mutators.t while this was being fixed).

is (await start { my @a; @a.push('n'); @a.append(('p1', 'p2')); @a.join(",") }),
    "n,p1,p2", "push then append on a worker-local array both land";

done-testing;

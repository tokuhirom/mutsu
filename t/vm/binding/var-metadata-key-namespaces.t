use v6;
use Test;

# Per-binding metadata (is this name a sigilless alias, is it `:=`-bound, does
# it carry a type or key-type constraint, is it a `state` variable, is its array
# shaped, does it have a cross-thread atomic lane) is not stored ON the binding:
# it lives as a sibling `__mutsu_<ns>::<name>` entry in the same env, and the
# only way to reach it is to build that derived key and probe with it.
#
# `MetaNs` (src/runtime/meta_ns.rs) is the single memoizing constructor for
# those keys, and issue #8087 moved every namespace that had a memoized helper
# off hand-built `format!` sites onto it. Two things can break silently in that
# move and in every later one:
#
#   * a mistyped prefix, which makes the writer and the reader disagree and the
#     metadata simply never be found (the unit tests in meta_ns.rs pin the
#     spellings; this file pins the behaviour they drive); and
#   * an env write that skips `note_env_key` — the latch that arms the
#     "could a key of this family exist?" probes in src/env.rs. A missed latch
#     leaves the probe permanently disabled, so the metadata is written and then
#     never read, which no type error and no unit test catches.
#
# Every case below is the behaviour of one namespace, written so that it fails
# if the key is written but not found.

plan 23;

# __mutsu_sigilless_readonly:: -- `my \x = 42` binds the value itself, so an
# assignment to it must be refused (the CheckReadOnly probe reads this key).
{
    my \ro = 42;
    is ro, 42, 'a sigilless term binds its value';
    dies-ok { EVAL 'my \x = 1; x = 2' }, 'assigning to a sigilless term is refused';
}

# __mutsu_sigilless_alias:: -- `:=` records the alias target under this key, and
# the write path walks the chain hop by hop.
{
    my $a = 1;
    my $b := $a;
    $b = 5;
    is $a, 5, 'a := alias writes through to its target';
    my $c := $b;
    $c = 9;
    is $a, 9, 'a two-hop alias chain still reaches the root';
}

# __mutsu_bound:: -- a `:=`-bound container is mutable in place, unlike a
# `constant`, and the readonly check tells them apart by this key.
{
    my @src = 1, 2, 3;
    my @bound := @src;
    @bound.push(4);
    is @src.elems, 4, 'a := bound array shares its source container';
    dies-ok { EVAL 'constant K = 1; K = 2' }, 'a constant is still immutable';
}

# __mutsu_bound_index:: -- an element that is itself `:=`-bound must not be
# replaced wholesale by an element store.
{
    my @a = 1, 2, 3;
    my $elem := @a[1];
    $elem = 20;
    is @a[1], 20, 'a bound element write reaches the array';
    @a[1] = 30;
    is $elem, 30, 'the binding survives a later plain element store';
}

# __mutsu_shaped_array_dims:: -- the declared dimensions of a shaped array.
{
    my @s[2;2];
    @s[1;1] = 9;
    is @s[1;1], 9, 'a shaped array stores through its declared dimensions';
    is @s.shape.join(','), '2,2', 'the declared shape is readable back';
    dies-ok { my @t[2]; @t[5] = 1 }, 'a shaped array refuses an out-of-range index';
}

# __mutsu_type:: -- a lexical's declared type constraint, probed on every store.
{
    my Int $typed = 1;
    is $typed, 1, 'a typed lexical accepts a conforming value';
    dies-ok { EVAL 'my Int $x; $x = "nope"' }, 'a typed lexical refuses a wrong-typed store';
}

# __mutsu_hash_key_type:: -- an object hash's key type (`my %h{Int}`).
{
    my %h{Int};
    %h{1} = 'one';
    is %h{1}, 'one', 'an object hash accepts a conforming key';
    dies-ok { EVAL 'my %h{Int}; %h{"s"} = 1' }, 'an object hash refuses a wrong-typed key';
}

# __mutsu_state_key:: -- the shared cell a `state` variable resolves to. Each
# closure clone gets its own; the key is what maps the name to the cell.
{
    sub counter() { state $n = 0; ++$n }
    counter(); counter();
    is counter(), 3, 'a state variable persists across calls';

    my $make = sub { my $c = sub { state $k = 0; ++$k }; $c };
    my $one = $make();
    my $two = $make();
    $one(); $one();
    is $two(), 1, 'a fresh closure clone gets its own state cell';
}

# __mutsu_callable_id:: -- the routine registration id that scopes the state
# store above, keyed by (package, name) rather than by name alone.
{
    package P1 { our sub tick() { state $n = 0; ++$n } }
    package P2 { our sub tick() { state $n = 100; ++$n } }
    P1::tick(); P1::tick();
    is P1::tick(), 3, 'a package-qualified sub keeps its own state';
    is P2::tick(), 101, 'a same-named sub in another package is a different routine';
}

# __mutsu_sigilless_alias:: on a parameter -- a raw/sigilless parameter aliases
# the caller's variable, so a write through it is visible at the call site.
{
    sub bump(\target) { target = target + 1 }
    my $v = 1;
    bump($v);
    is $v, 2, 'a raw parameter writes back to the caller';

    sub pair-up([\a, \b]) { "{a}-{b}" }
    is pair-up([1, 2]), '1-2', 'a sigilless sub-signature destructure binds both leaves';
}

# __mutsu_atomic_arr:: / __mutsu_atomic_hash:: -- the cross-thread lane a shared
# container resolves to once concurrent code touches it. Without the lane, one
# worker's snapshot clobbers the other's appends.
{
    my @shared;
    my @promises = (^4).map: -> $i {
        start { @shared.push($i) }
    };
    await @promises;
    is @shared.elems, 4, 'concurrent pushes all land in the shared array';

    my %seen;
    await (^4).map: -> $i { start { %seen{$i} = $i * 2 } };
    is %seen.elems, 4, 'concurrent hash element writes all land';
}

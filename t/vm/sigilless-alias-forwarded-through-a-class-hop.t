use Test;

# A sigilless parameter (`\c`) IS the caller's container, so forwarding it on
# to another routine must keep that identity however many hops it takes and
# whatever kind of routine each hop is.
#
# A SUB callee always worked: sub frames chain env, so the exit writeback's
# "resolve to the root of the alias chain" reached the original variable. A
# METHOD callee does not chain env -- its writeback merges only into the frame
# that called it, where the chain's root is not a key at all -- so the write
# landed on a phantom entry, and the intermediate frame then reached its own
# exit still holding the pre-call value and clobbered the root with it. Every
# `method go(\c) { Other.go(c) }` therefore lost the callee's write entirely.
#
# That is `Crane.add/set/remove($container, :path(), ..., :in-place)`, which is
# a chain of class methods forwarding one `\container`.
#
# Separately: a raw binding to an `@`/`%` variable is that aggregate, so
# `c = LIST` is `@a.STORE(LIST)` and the caller keeps an Array/Hash rather
# than the bare List the parameter's slot held.
#
# Every expectation below is rakudo v2026.07's own answer.

plan 16;

class Leaf { method go(\c) { c = 1 } }
class LeafRw { method go($c is rw) { $c = 1 } }
sub leaf-sub(\c) { c = 1 }

# --- forwarding a raw parameter to a method ------------------------------

{
    class Mid { method go(\c) { Leaf.go(c) } }
    my $v;
    Mid.go($v);
    is($v, 1, 'method -> method: the callee write reaches the original');
}

{
    class MidRw { method go(\c) { LeafRw.go(c) } }
    my $v;
    MidRw.go($v);
    is($v, 1, 'method -> is-rw method: the callee write reaches the original');
}

{
    sub mid-sub(\c) { Leaf.go(c) }
    my $v;
    mid-sub($v);
    is($v, 1, 'sub -> method: the callee write reaches the original');
}

{
    class Deep1 { method go(\c) { Leaf.go(c) } }
    class Deep2 { method go(\c) { Deep1.go(c) } }
    my $v;
    Deep2.go($v);
    is($v, 1, 'three method hops still reach the original');
}

# The intermediate frame's own alias sees it too, not just the root.
{
    class MidSeen {
        method go(\c) {
            Leaf.go(c);
            c;
        }
    }
    my $v;
    is(MidSeen.go($v), 1, 'the intermediate frame reads the new value after the call');
    is($v, 1, 'and the original has it as well');
}

# --- the shapes that already worked must keep working --------------------

{
    sub mid-both(\c) { leaf-sub(c) }
    my $v;
    mid-both($v);
    is($v, 1, 'sub -> sub still reaches the original');
}

{
    class Own { method go { my $v; Leaf.go($v); $v } }
    is(Own.go, 1, 'a method passing its own lexical still works');
}

{
    my $v;
    Leaf.go($v);
    is($v, 1, 'a mainline lexical passed straight to a method still works');
}

# --- a raw alias of an aggregate stores through the aggregate ------------

sub store-list(\c) { c = ('x', 'y') }
sub store-pairs(\c) { c = (:a(1), :b(2)) }

{
    my @a;
    store-list(@a);
    is-deeply(@a, ['x', 'y'], 'sub: `c = LIST` on an @-alias stores into the Array');
}

{
    my %h;
    store-pairs(%h);
    is-deeply(%h, {:a(1), :b(2)}, 'sub: `c = PAIRS` on a %-alias stores into the Hash');
}

{
    class StoreList { method go(\c) { c = ('x', 'y') } }
    my @a;
    StoreList.go(@a);
    is-deeply(@a, ['x', 'y'], 'method: `c = LIST` on an @-alias stores into the Array');
}

{
    sub relay(\c) { store-list(c) }
    my @a;
    relay(@a);
    is-deeply(@a, ['x', 'y'], 'sub -> sub: an @-alias relayed one hop still stores a flat Array');
}

{
    class RelayB { method go(\c) { c = ('x', 'y') } }
    class RelayA { method go(\c) { RelayB.go(c) } }
    my @a;
    RelayA.go(@a);
    is-deeply(@a, ['x', 'y'], 'method -> method: an @-alias relayed one hop still stores a flat Array');
}

# A `$`-sigiled target keeps whatever it was given -- no aggregate shaping.
{
    sub store-scalar(\c) { c = ('x', 'y') }
    my $s;
    store-scalar($s);
    is-deeply($s, ('x', 'y'), 'a $-sigiled target keeps the List it was assigned');
}

{
    my @a = 1, 2, 3;
    sub read-only(\c) { c.elems }
    is(read-only(@a), 3, 'a read through a raw alias is unaffected');
}

# done

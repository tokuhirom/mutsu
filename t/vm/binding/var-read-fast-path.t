use v6;
use Test;

# `GetLocal` takes a narrow path in `exec_get_local_op_inner` (#8332) instead of
# the twelve-guard preamble every local read used to pay. The path is admitted
# by three conditions: the slot's NAME decides every name-shaped guard against
# it (`CompiledCode::local_read_plain`), no runtime mechanism the preamble looks
# for has ever been created anywhere in the process
# (`vm_jit::local_read_unspoiled`), and the slot's own word is none of the kinds
# the arm's tail still inspects (`Value::is_plain_local_read`).
#
# Every block below stands in for one of those guards and pins that the answer
# is what it was when every read fell through the whole preamble.
#
# NOTE ON ORDERING: the spoiler latch is process-global and monotonic, so the
# first `:=`, Proxy or atomic in this file arms it for everything after. The
# plain-read blocks are therefore FIRST — they are the ones that actually
# execute the fast path. The later blocks pin the paths it must decline.

plan 27;

# -- the fast path itself: ordinary reads of ordinary scalars --
{
    my $i = 0;
    $i = $i + 1;
    $i = $i + 1;
    is $i, 2, 'a plain scalar reads back its latest value';
    my $s = 'ab';
    is $s ~ $s, 'abab', 'a Str-valued local reads back';
    my $n = 1.5;
    is $n + $n, 3.0, 'a Num-valued local reads back';
    my $r = 1/3;
    is $r * 3, 1, 'a Rat-valued local reads back';
    my $b = True;
    ok $b, 'a Bool-valued local reads back';
    my $t = Int;
    ok $t === Int, 'a type object in a local reads back by identity';
}

# -- a read is a *copy*, so the reader cannot mutate the slot --
{
    my $a = [1, 2];
    my $c = $a;
    $c.push(3);
    is $a.elems, 3, 'a scalar-held Array is shared, not copied, by a read';
    my $x = 1;
    my $y = $x;
    $x = 9;
    is $y, 1, 'a read of an immutable value does not alias the slot';
}

# -- `&` and sigilless names are plain slots too --
{
    my &f = -> $v { $v * 2 };
    is f(3), 6, 'an &-sigil local reads back as a callable';
    my \bare = 41;
    is bare + 1, 42, 'a sigilless binding reads back';
}

# -- the loop topic is a plain slot name, and still tracks each iteration --
{
    my @seen;
    for 1, 2, 3 { @seen.push($_) }
    is @seen.join(','), '1,2,3', 'the topic reads the current item on each pass';
}

# -- an undeclared read still throws, and Nil still reaches its own arm --
{
    eval-dies-ok 'say $undeclared_thing_ffff', 'reading an undeclared variable throws';
    my $u;
    ok $u === Any, 'an uninitialized scalar reads as Any';
    my $d is default('N/A');
    is $d, 'N/A', 'a scalar with is default() reads its default';
}

# -- @ / % slots are NOT plain: the sigil-keyed probes must still run --
{
    my @a = 1, 2, 3;
    is @a.join(','), '1,2,3', 'an @-sigil local reads back';
    is @a[1], 2, 'an @-sigil local indexes';
    my %h = :k(1);
    is %h<k>, 1, 'a %-sigil local reads back';
}

# -- attribute slots are NOT plain: the read goes through self's cell --
{
    class C {
        has $.v;
        has @.l;
        method priv() { $!v }
        method pub() { $.v }
        method list() { @!l.elems }
    }
    my $o = C.new(v => 7, l => [1, 2]);
    is $o.priv, 7, 'a private-attribute read reaches the instance';
    is $o.pub, 7, 'a public-attribute read reaches the instance';
    is $o.list, 2, 'an @-sigil attribute read reaches the instance';
    dies-ok { C.priv }, 'a private-attribute read on a type object throws';
}

# -- a lazily-resolved slot word declines the fast path --
{
    my %h;
    my $e := %h<k>;
    $e = 9;
    is %h<k>, 9, 'a deferred hash-entry bind materializes on write';
    is $e, 9, 'and reads back through the same token';
}

# -- a shared cell declines it, in both directions --
{
    my $src = 1;
    my $alias := $src;
    $src = 9;
    is $alias, 9, 'a := alias observes a write to its source';
    my $acc = 0;
    sub bump() { $acc = $acc + 1 }
    bump();
    bump();
    is $acc, 2, 'a named sub writing an outer scalar accumulates in the owner';
}

# -- a Proxy declines it: the read must run FETCH, not hand back the word --
{
    my $backing = 0;
    my $p := Proxy.new(
        FETCH => method () { $backing * 10 },
        STORE => method ($v) { $backing = $v },
    );
    $p = 4;
    is $p, 40, 'a Proxy-bound scalar FETCHes on read';
}

# -- an atomic variable declines it: the read must fetch the atomic cell --
{
    my atomicint $c = 0;
    $c⚛++;
    $c⚛++;
    is $c, 2, 'an atomic variable reads its accumulated value';
}

done-testing;

use v6;
use Test;

# `$x = <ordinary scalar>` takes a narrow path in `exec_set_local_op`
# (#8094) instead of the ~2,000-line store-flavour cascade. Every guard that
# path asks stands in for one branch of the cascade, so this file drives a
# plain scalar store through each of those branches and pins that the answer is
# the same as it was when every store fell through the whole thing.

plan 28;

# -- the fast path itself: a plain reassignment of an already-declared scalar --
{
    my $i = 0;
    $i = $i + 1;
    $i = $i + 1;
    is $i, 2, 'plain scalar reassignment accumulates';
    my $s = 'a';
    $s = $s ~ 'b';
    is $s, 'ab', 'plain string reassignment accumulates';
}

# -- itemization still happens on the way in --
{
    my $a;
    $a = [1, 2];
    is $a.raku, '$[1, 2]', 'an Array stored into a scalar is itemized';
    my $h;
    $h = {:k(1)};
    is $h.raku, '${:k(1)}', 'a Hash stored into a scalar is itemized';
    my $r;
    $r = 1..3;
    my @flat = $r;
    is @flat.elems, 1, 'a Range stored into a scalar is one element in list context';
    my $q;
    $q = (1, 2, 3).Seq;
    is $q.raku, '$((1, 2, 3).Seq)', 'a Seq stored into a scalar is itemized';
}

# -- Nil resets an untyped scalar to Any, a typed one to its type object --
{
    my $x = 5;
    $x = Nil;
    ok $x === Any, 'Nil assigned to an untyped scalar resets it to Any';
    my Str $t = 'a';
    $t = Nil;
    ok $t === Str, 'Nil assigned to a typed scalar resets it to its type object';
}

# -- a typed lexical is still type-checked and coerced --
{
    my Int $n = 1;
    $n = 2;
    is $n, 2, 'a typed scalar takes a conforming store';
    dies-ok { $n = 'x' }, 'a typed scalar rejects a non-conforming store';
    my Num $f = 1e0;
    $f = 2e0;
    is $f, 2e0, 'a typed Num scalar takes a conforming store';
}

# -- `is default(...)` survives a Nil store --
{
    my $d is default('N/A') = 'v';
    $d = Nil;
    is $d, 'N/A', 'a scalar with is default() keeps its default over a Nil store';
}

# -- `:=` aliases still see the write, in both directions --
{
    my $src = 1;
    my $alias := $src;
    $src = 9;
    is $alias, 9, 'a := alias observes a write to its source';
    $alias = 11;
    is $src, 11, 'the source observes a write through the := alias';
}

# -- a scalar bound to a shared cell is written THROUGH, not replaced --
{
    my @a = 1, 2;
    my $e := @a[0];
    $e = 7;
    is @a[0], 7, 'a store through an element-bound scalar reaches the array';
}

# -- a Proxy in the slot still runs its STORE --
{
    my $seen;
    my $p := Proxy.new(FETCH => sub ($) { 42 }, STORE => sub ($, $v) { $seen = $v });
    $p = 5;
    is $seen, 5, 'a store into a Proxy-bound scalar runs STORE';
    is $p, 42, 'the Proxy still FETCHes';
}

# -- an atomic variable's cell is detached by a plain reassignment --
{
    my atomicint $c = 0;
    $c⚛++;
    is $c, 1, 'atomic increment reaches the variable';
    $c = 5;
    is $c, 5, 'a plain store to an atomic variable takes effect';
}

# -- a `term:<...>` definition mirrors itself, and is not a plain scalar slot --
{
    my \unused = 1;
    is unused, 1, 'a sigilless binding reads back';
}

# -- the topic is not a plain scalar slot: writing it reaches its source --
{
    my $t = 1;
    given $t { $_ = 4 }
    is $t, 4, 'assigning the topic writes back through its source variable';
}

# -- an attribute write still mirrors into self's cell --
{
    class C {
        has $.v is rw;
        method bump() { $!v = $!v + 1; self }
    }
    my $o = C.new(v => 1);
    $o.bump;
    is $o.v, 2, 'a private-attribute store mirrors into the instance';
}

# -- `our` in the same chunk keeps its package mirror in sync --
{
    our $g = 1;
    $g = 3;
    is $GLOBAL::g, 3, 'a store to an our-variable reaches its package slot';
}

# -- a closure capturing the variable sees later writes --
{
    my $n = 1;
    my $get = { $n };
    $n = 8;
    is $get(), 8, 'a closure reads the latest value of a captured scalar';
}

# -- a state variable accumulates across calls --
{
    sub counter() { state $s = 0; $s = $s + 1; $s }
    counter();
    counter();
    is counter(), 3, 'a state scalar accumulates across calls';
}

# -- a re-store of the same backing array keeps its identity --
{
    my @a = 1, 2;
    my $v = @a;
    $v = $v;
    is $v.raku, '$[1, 2]', 'restoring a scalar-held array preserves its shape';
}

# -- a Failure stored under `use fatal` still throws --
{
    my $ok = 0;
    {
        use fatal;
        try {
            my $f = Nil;
            $f = (die 'boom');
            CATCH { default { $ok = 1 } }
        }
    }
    is $ok, 1, 'a die on the RHS of a scalar store propagates under use fatal';
}

# -- writes from a nested named sub reach the owner's slot --
{
    my $acc = 0;
    sub via() { $acc = $acc + 1 }
    via();
    via();
    is $acc, 2, 'a named sub writing an outer scalar accumulates in the owner';
}

done-testing;

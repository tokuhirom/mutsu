use Test;
use nqp;

# The integer atomics (`⚛++`, `⚛+=`, `atomic-fetch-add`, `nqp::atomicinc_i`, ...)
# act on a native integer container: an `int` / `atomicint` variable or
# attribute, or an element of a native-int array. A plain `my $x` (or `my Int
# $x`, `my @a`, `%h<k>`) is refused. `atomic-fetch` / `atomic-assign` / `cas`
# keep their `$target is rw` candidates and stay legal on any scalar (#11834).
# Every expected answer is Rakudo 2026.09's.

my $nomatch = /'Cannot resolve caller ' .* '; the following candidates' \s+
               'match the type but require mutable arguments:' \s+
               '(atomicint $target is rw'/;

# ---- a plain scalar is refused by every integer atomic routine -----------

{
    my $x = 1;
    throws-like { $x⚛++ }, X::Multi::NoMatch, message => $nomatch, 'postfix ⚛++ on a plain scalar';
    throws-like { ++⚛$x }, X::Multi::NoMatch, message => $nomatch, 'prefix ++⚛';
    throws-like { $x⚛-- }, X::Multi::NoMatch, message => $nomatch, 'postfix ⚛--';
    throws-like { --⚛$x }, X::Multi::NoMatch, message => $nomatch, 'prefix --⚛';
    throws-like { $x ⚛+= 2 }, X::Multi::NoMatch, message => $nomatch, '⚛+=';
    throws-like { $x ⚛-= 2 }, X::Multi::NoMatch, message => $nomatch, '⚛-=';
    throws-like { atomic-fetch-inc($x) }, X::Multi::NoMatch, message => $nomatch, 'atomic-fetch-inc';
    throws-like { atomic-fetch-dec($x) }, X::Multi::NoMatch, message => $nomatch, 'atomic-fetch-dec';
    throws-like { atomic-inc-fetch($x) }, X::Multi::NoMatch, message => $nomatch, 'atomic-inc-fetch';
    throws-like { atomic-dec-fetch($x) }, X::Multi::NoMatch, message => $nomatch, 'atomic-dec-fetch';
    throws-like { atomic-fetch-add($x, 2) }, X::Multi::NoMatch, message => $nomatch, 'atomic-fetch-add';
    throws-like { atomic-fetch-sub($x, 2) }, X::Multi::NoMatch, message => $nomatch, 'atomic-fetch-sub';
    throws-like { atomic-add-fetch($x, 2) }, X::Multi::NoMatch, message => $nomatch, 'atomic-add-fetch';
    throws-like { atomic-sub-fetch($x, 2) }, X::Multi::NoMatch, message => $nomatch, 'atomic-sub-fetch';
    is $x, 1, 'a refused atomic leaves the variable alone';
}

# The message names the routine as written and the arguments' types.
{
    my $x = 1;
    my $u;
    my Int $t;
    my Str $s = 'a';
    throws-like { $x⚛++ }, X::Multi::NoMatch, message => /'postfix:<⚛++>(Int:D)'/, 'operator spelling, Int:D';
    throws-like { $x ⚛+= 2 }, X::Multi::NoMatch, message => /'infix:<⚛+=>(Int:D, Int:D)'/, 'two arguments';
    throws-like { atomic-fetch-add($x, 2) }, X::Multi::NoMatch,
        message => /'atomic-fetch-add(Int:D, Int:D)'/ & /'(atomicint $target is rw, int $add --> atomicint)'/
            & /'(atomicint $target is rw, Int:D $add --> atomicint)'/ & /'(atomicint $target is rw, $add --> atomicint)'/,
        'the routine name and all three candidates';
    throws-like { $u⚛++ }, X::Multi::NoMatch, message => /'(Any:U)'/, 'an uninitialised scalar is Any:U';
    throws-like { $t⚛++ }, X::Multi::NoMatch, message => /'(Int:U)'/, 'a typed, uninitialised scalar is Int:U';
    throws-like { $s⚛++ }, X::Multi::NoMatch, message => /'(Str:D)'/, 'a Str scalar is Str:D';
}

# ---- a boxed type does not make it native --------------------------------

{
    my Int $x = 1;
    throws-like { $x⚛++ }, X::Multi::NoMatch, message => $nomatch, 'my Int $x';
    throws-like { atomic-fetch-add($x, 1) }, X::Multi::NoMatch, message => $nomatch, '... atomic-fetch-add';
    my uint $u = 1;
    throws-like { $u⚛++ }, X::Multi::NoMatch, message => $nomatch, 'my uint $u: unsigned is not an atomicint';
}

# ---- the plain-scalar routines stay legal --------------------------------

{
    my $x = 1;
    is atomic-fetch($x), 1, 'atomic-fetch on a plain scalar';
    is ⚛$x, 1, 'prefix ⚛ on a plain scalar';
    $x ⚛= 5;
    is $x, 5, '⚛= on a plain scalar';
    atomic-assign($x, 6);
    is $x, 6, 'atomic-assign on a plain scalar';
    is cas($x, 6, 7), 6, 'cas on a plain scalar answers the old value';
    is $x, 7, '... and swaps it';
    is cas($x, * + 1), 8, 'cas with a code block';
    is nqp::atomicload($x), 8, 'nqp::atomicload on a plain scalar';
    is nqp::atomicstore($x, 9), 9, 'nqp::atomicstore on a plain scalar';
    is nqp::cas($x, 9, 10), 9, 'nqp::cas on a plain scalar';
}

# ---- native scalars work -------------------------------------------------

{
    my int $i = 1;
    is $i⚛++, 1, 'postfix ⚛++ on an int';
    is $i, 2, '... incremented it';
    is ++⚛$i, 3, 'prefix ++⚛ on an int';
    $i ⚛+= 4;
    is $i, 7, '⚛+= on an int';
    is atomic-fetch-add($i, 3), 7, 'atomic-fetch-add on an int';
    is atomic-sub-fetch($i, 10), 0, 'atomic-sub-fetch on an int';

    my atomicint $a = 1;
    is $a⚛++, 1, 'postfix ⚛++ on an atomicint';
    $a ⚛-= 2;
    is $a, 0, '⚛-= on an atomicint';
    is atomic-inc-fetch($a), 1, 'atomic-inc-fetch on an atomicint';

    my int64 $l = 5;
    is $l⚛--, 5, 'postfix ⚛-- on an int64';
    is $l, 4, '... decremented it';
}

# ---- a closure and a nested scope see the declaration they close over ----

{
    my $plain = 1;
    my atomicint $native = 1;
    my $bump-plain = { $plain⚛++ };
    my $bump-native = { $native⚛++ };
    throws-like { $bump-plain() }, X::Multi::NoMatch, message => $nomatch, 'a closure over a plain scalar';
    is $bump-native(), 1, 'a closure over an atomicint';
    is $native, 2, '... which it incremented';

    sub bump-plain { $plain⚛++ }
    sub bump-native { $native⚛++ }
    throws-like { bump-plain() }, X::Multi::NoMatch, message => $nomatch, 'a sub over a plain scalar';
    is bump-native(), 2, 'a sub over an atomicint';
}

{
    # A `my` in an inner block shadows; leaving the block restores the outer one.
    my atomicint $x = 10;
    {
        my $x = 1;
        throws-like { $x⚛++ }, X::Multi::NoMatch, message => $nomatch, 'a plain shadow of an atomicint';
    }
    is $x⚛++, 10, 'the atomicint is visible again after the block';
    my $y = 1;
    {
        my atomicint $y = 20;
        lives-ok { $y⚛++ }, 'an atomicint shadow of a plain scalar is accepted';
    }
    throws-like { $y⚛++ }, X::Multi::NoMatch, message => $nomatch, 'the plain scalar is visible again';
}

# ---- an `is rw` parameter carries its caller's container -----------------

{
    sub bump-rw($p is rw) { $p⚛++ }
    sub bump-ro($p) { $p⚛++ }
    sub bump-typed(atomicint $p is rw) { $p⚛++ }
    my $plain = 1;
    my atomicint $native = 1;
    throws-like { bump-rw($plain) }, X::Multi::NoMatch, message => $nomatch, 'an is rw parameter bound to a plain scalar';
    is $plain, 1, '... leaves it alone';
    is bump-rw($native), 1, 'an is rw parameter bound to an atomicint';
    is $native, 2, '... incremented the caller\'s variable';
    is bump-typed($native), 2, 'an atomicint is rw parameter';
    is $native, 3, '... incremented the caller\'s variable';
    throws-like { bump-ro($native) }, X::Multi::NoMatch, message => $nomatch, 'a read-only parameter is not an atomicint container';
}

# ---- array and hash elements ---------------------------------------------

{
    my @plain = 0;
    my Int @boxed = 0;
    my %hash = a => 0;
    throws-like { @plain[0]⚛++ }, X::Multi::NoMatch, message => $nomatch, 'an element of a plain array';
    throws-like { @boxed[0]⚛++ }, X::Multi::NoMatch, message => $nomatch, 'an element of an Int array';
    throws-like { %hash<a>⚛++ }, X::Multi::NoMatch, message => $nomatch, 'a hash element';
    throws-like { atomic-fetch-add(@plain[0], 2) }, X::Multi::NoMatch, message => $nomatch, 'atomic-fetch-add on an element';
    is-deeply @plain, [0], '... the element is untouched';
    is-deeply %hash, {a => 0}, '... and so is the hash';
    is atomic-fetch(@plain[0]), 0, 'atomic-fetch on a plain element stays legal';
    is atomic-assign(@plain[0], 4), 4, 'atomic-assign on a plain element stays legal';
    is cas(@plain[0], 4, 5), 4, 'cas on a plain element stays legal';
    is-deeply @plain, [5], '... and wrote it';

    my int @int = 0;
    my atomicint @atomic = 0;
    @int[0]⚛++;
    @atomic[0]⚛++;
    atomic-add-fetch(@atomic[0], 4);
    is-deeply @int, array[int].new(1), 'an element of an int array';
    is-deeply @atomic, array[atomicint].new(5), 'an element of an atomicint array';
}

# ---- attributes ----------------------------------------------------------

{
    class Counters {
        has $.plain is rw = 0;
        has Int $.boxed = 0;
        has int $.native = 0;
        has atomicint $.atomic = 0;
        method bump-plain { $!plain⚛++ }
        method bump-boxed { $!boxed ⚛+= 2 }
        method bump-native { $!native⚛++ }
        method bump-atomic { $!atomic ⚛+= 3 }
        method fetch-plain { atomic-fetch($!plain) }
    }
    my $c = Counters.new;
    throws-like { $c.bump-plain }, X::Multi::NoMatch, message => $nomatch, 'a plain attribute';
    throws-like { $c.bump-boxed }, X::Multi::NoMatch, message => $nomatch, 'an Int attribute';
    is $c.bump-native, 0, 'an int attribute';
    is $c.bump-atomic, 3, 'an atomicint attribute';
    is $c.native, 1, '... incremented the int attribute';
    is $c.atomic, 3, '... and the atomicint attribute';
    is $c.fetch-plain, 0, 'atomic-fetch on a plain attribute stays legal';
}

# ---- the nqp ops: MoarVM's message at the nqp level ----------------------

{
    my $nqp-msg = /'Can only do integer atomic operations on a container referencing a native integer'/;
    my $x = 1;
    my @plain = 0;
    throws-like { nqp::atomicinc_i($x) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicinc_i on a plain scalar';
    throws-like { nqp::atomicdec_i($x) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicdec_i';
    throws-like { nqp::atomicadd_i($x, 3) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicadd_i';
    throws-like { nqp::atomicload_i($x) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicload_i';
    throws-like { nqp::atomicstore_i($x, 3) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicstore_i';
    throws-like { nqp::cas_i($x, 1, 3) }, X::AdHoc, message => $nqp-msg, 'nqp::cas_i';
    throws-like { nqp::atomicinc_i(@plain[0]) }, X::AdHoc, message => $nqp-msg, 'nqp::atomicinc_i on a plain element';
    is $x, 1, 'a refused nqp op leaves the variable alone';

    my int $i = 1;
    is nqp::atomicinc_i($i), 1, 'nqp::atomicinc_i on an int';
    is nqp::atomicadd_i($i, 5), 2, 'nqp::atomicadd_i on an int';
    is nqp::atomicload_i($i), 7, 'nqp::atomicload_i on an int';
    is nqp::cas_i($i, 7, 8), 7, 'nqp::cas_i on an int';
    is $i, 8, '... and the int holds the swapped value';
}

# ---- concurrency still counts every increment ----------------------------

{
    my atomicint $n = 0;
    await (^4).map: { start { $n⚛++ for ^250 } };
    is $n, 1000, 'four threads incrementing an atomicint lose no update';
}

done-testing;

use Test;
use nqp;

# MoarVM refuses EVERY atomic operation on a native integer narrower than the
# machine word (`int8` / `int16` / `int32`): Rakudo's `atomicint $target is rw`
# candidate takes the container, and the VM then declines it (#12008). That
# covers the lenient forms (`atomic-fetch`, `⚛=`, `cas`) as well as the
# read-modify-write ones, and the message depends on where the container lives.
# `int`, `atomicint` and `int64` are fine; `uint` and `byte` are a dispatch
# failure (#11834). Every expected answer is Rakudo 2026.09's.

my $lexical = /"Cannot atomic load from an integer lexical not of the machine's native size"/;
my $element = /'Can only do integer atomic operation on native integer array element of atomic size'/;
my $attribute = /'Can only do an atomic integer operation on an atomicint attribute'/;

# ---- a narrow lexical ------------------------------------------------------

{
    my int32 $x = 1;
    throws-like { $x⚛++ }, X::AdHoc, message => $lexical, 'postfix ⚛++';
    throws-like { ++⚛$x }, X::AdHoc, message => $lexical, 'prefix ++⚛';
    throws-like { $x⚛-- }, X::AdHoc, message => $lexical, 'postfix ⚛--';
    throws-like { $x ⚛+= 2 }, X::AdHoc, message => $lexical, '⚛+=';
    throws-like { $x ⚛-= 2 }, X::AdHoc, message => $lexical, '⚛-=';
    throws-like { atomic-fetch-add($x, 2) }, X::AdHoc, message => $lexical, 'atomic-fetch-add';
    throws-like { atomic-add-fetch($x, 2) }, X::AdHoc, message => $lexical, 'atomic-add-fetch';
    throws-like { atomic-fetch-inc($x) }, X::AdHoc, message => $lexical, 'atomic-fetch-inc';
    # The lenient forms, which accept any scalar, refuse a narrow one too.
    throws-like { ⚛$x }, X::AdHoc, message => $lexical, 'prefix ⚛ (fetch)';
    throws-like { $x ⚛= 5 }, X::AdHoc, message => $lexical, '⚛= (store)';
    throws-like { atomic-fetch($x) }, X::AdHoc, message => $lexical, 'atomic-fetch';
    throws-like { atomic-assign($x, 3) }, X::AdHoc, message => $lexical, 'atomic-assign';
    throws-like { cas($x, 1, 2) }, X::AdHoc, message => $lexical, 'cas';
    is $x, 1, 'a refused atomic leaves the variable alone';
}

{
    my int8 $a = 1;
    my int16 $b = 1;
    throws-like { $a⚛++ }, X::AdHoc, message => $lexical, 'int8';
    throws-like { $b⚛++ }, X::AdHoc, message => $lexical, 'int16';
    throws-like { nqp::atomicinc_i($a) }, X::AdHoc, message => $lexical, 'nqp::atomicinc_i on an int8';
    throws-like { nqp::atomicload_i($b) }, X::AdHoc, message => $lexical, 'nqp::atomicload_i on an int16';
    throws-like { nqp::cas_i($a, 1, 2) }, X::AdHoc, message => $lexical, 'nqp::cas_i on an int8';
}

# A narrow `is rw` parameter reaches the refusal too.
{
    sub bump(int32 $p is rw) { $p⚛++ }
    sub peek(int32 $p is rw) { atomic-fetch($p) }
    my int32 $x = 1;
    throws-like { bump($x) }, X::AdHoc, message => $lexical, 'a narrow is-rw parameter';
    throws-like { peek($x) }, X::AdHoc, message => $lexical, '... through a lenient routine';
}

# ---- the machine-size integers are untouched ------------------------------

{
    my int $i = 1;
    my atomicint $a = 1;
    my int64 $w = 1;
    $i⚛++;
    $a⚛++;
    $w⚛++;
    is $i, 2, 'int';
    is $a, 2, 'atomicint';
    is $w, 2, 'int64';
    is ⚛$i, 2, 'a lenient fetch of an int';
    $a ⚛= 7;
    is atomic-fetch($a), 7, 'a lenient store and fetch of an atomicint';
    is cas($w, 2, 9), 2, 'cas on an int64';
    is $w, 9, '... swapped it';
}

# An unsigned type is still a dispatch failure, not the narrow refusal.
{
    my uint $u = 1;
    throws-like { $u⚛++ }, X::Multi::NoMatch, message => /'Cannot resolve caller postfix:<⚛++>'/, 'uint';
}

# The lenient forms stay legal on any plain scalar.
{
    my $plain = 1;
    my Int $boxed = 1;
    is atomic-fetch($plain), 1, 'atomic-fetch on a plain scalar';
    is atomic-assign($boxed, 4), 4, 'atomic-assign on a typed scalar';
    is cas($plain, 1, 5), 1, 'cas on a plain scalar';
    is $plain, 5, '... swapped it';
}

# ---- an element of a narrow array ------------------------------------------

{
    my int32 @a = 1, 2;
    throws-like { @a[0]⚛++ }, X::AdHoc, message => $element, 'postfix ⚛++ on an element';
    throws-like { @a[1] ⚛+= 3 }, X::AdHoc, message => $element, '⚛+= on an element';
    throws-like { @a[1] ⚛-= 3 }, X::AdHoc, message => $element, '⚛-= on an element';
    throws-like { atomic-add-fetch(@a[1], 3) }, X::AdHoc, message => $element, 'atomic-add-fetch on an element';
    throws-like { atomic-fetch-add(@a[0], 2) }, X::AdHoc, message => $element, 'atomic-fetch-add on an element';
    throws-like { ⚛@a[0] }, X::AdHoc, message => $element, 'prefix ⚛ on an element';
    throws-like { @a[0] ⚛= 3 }, X::AdHoc, message => $element, '⚛= on an element';
    throws-like { atomic-fetch(@a[0]) }, X::AdHoc, message => $element, 'atomic-fetch on an element';
    throws-like { atomic-assign(@a[0], 3) }, X::AdHoc, message => $element, 'atomic-assign on an element';
    throws-like { cas(@a[0], 1, 2) }, X::AdHoc, message => $element, 'cas on an element';
    throws-like { nqp::atomicinc_i(@a[0]) }, X::AdHoc, message => $element, 'nqp::atomicinc_i on an element';
    is-deeply @a, array[int32].new(1, 2), 'a refused atomic leaves the array alone';
}

{
    my int8 @b = 1;
    throws-like { @b[0]⚛++ }, X::AdHoc, message => $element, 'an int8 array';
    my int @ok = 1, 2;
    @ok[0]⚛++;
    is-deeply @ok, array[int].new(2, 2), 'an int array is fine';
    is atomic-fetch(@ok[1]), 2, '... for a lenient routine too';
}

# ---- an attribute of a narrow type -----------------------------------------

{
    my class Counter {
        has int16 $!narrow = 1;
        has int $!wide = 1;
        method inc { $!narrow⚛++ }
        method fetch { ⚛$!narrow }
        method store { $!narrow ⚛= 3 }
        method lenient-fetch { atomic-fetch($!narrow) }
        method lenient-store { atomic-assign($!narrow, 3) }
        method swap { cas($!narrow, 1, 2) }
        method wide-inc { $!wide⚛++ }
        method wide-fetch { ⚛$!wide }
    }
    my $c = Counter.new;
    throws-like { $c.inc }, X::AdHoc, message => $attribute, 'postfix ⚛++ on a narrow attribute';
    throws-like { $c.fetch }, X::AdHoc, message => $attribute, 'prefix ⚛ on a narrow attribute';
    throws-like { $c.store }, X::AdHoc, message => $attribute, '⚛= on a narrow attribute';
    throws-like { $c.lenient-fetch }, X::AdHoc, message => $attribute, 'atomic-fetch on a narrow attribute';
    throws-like { $c.lenient-store }, X::AdHoc, message => $attribute, 'atomic-assign on a narrow attribute';
    throws-like { $c.swap }, X::AdHoc, message => $attribute, 'cas on a narrow attribute';
    is $c.wide-inc, 1, 'an int attribute is fine';
    is $c.wide-fetch, 2, '... and its lenient fetch';
}

# ---- the code form of cas refuses before its block runs --------------------

{
    my class Holder {
        has int16 $!p = 1;
        method swap($ran is rw) { cas($!p, -> $v { $ran++; $v + 1 }) }
    }
    my $ran = 0;
    throws-like { Holder.new.swap($ran) }, X::AdHoc, message => $attribute,
        'the code form of cas on a narrow attribute';
    is $ran, 0, '... refuses before its block runs';

    my int16 @a = 1, 2;
    my $ran2 = 0;
    throws-like { cas(@a[0], -> $v { $ran2++; $v + 1 }) }, X::AdHoc, message => $element,
        'the code form of cas on a narrow element';
    is $ran2, 0, '... also before its block runs';
}

done-testing;

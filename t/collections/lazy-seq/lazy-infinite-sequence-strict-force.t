use Test;

plan 28;

# Every sequence-spec lazy list (an unbounded Range stepped by `.succ`, an
# arithmetic/geometric `...` sequence, `.roll(*)`) is infinite. A strict
# force of one used to stop silently at a 100000-element prefix and hand that
# back as the whole list (#10846). It now throws X::Cannot::Lazy, the same
# verdict a strict force of an infinite `.map` pipe reaches, while the front
# mutators keep the array lazy as Rakudo does.

# Draining an infinite source through the Iterator protocol does not truncate.
throws-like { my $i = (1..*).iterator; my @o; $i.push-all(@o) }, X::Cannot::Lazy,
    'push-all on an unbounded Range iterator throws (the #10846 repro)';

{
    my @s = 1..*;
    my @o;
    throws-like { @s.iterator.push-all(@o) }, X::Cannot::Lazy,
        'push-all over an infinite .succ sequence throws';
    my $i = (1, 2, 4 ... *).iterator;
    throws-like { $i.push-all(my @p) }, X::Cannot::Lazy,
        'push-all over an infinite geometric sequence throws';
    lives-ok { (1, 3 ... *).iterator.sink-all },
        'sink-all over a side-effect-free infinite sequence generates nothing';
}

{
    my @a = 1..*;
    throws-like { @a.eager }, X::Cannot::Lazy, '.eager on an infinite lazy array throws';
}

# A bounded pull still works.
{
    my $i = (1, 2, 4 ... *).iterator;
    $i.push-exactly(my @o, 4);
    is-deeply @o, [1, 2, 4, 8], 'push-exactly pulls a bounded prefix';
}

# Front mutators keep the array lazy instead of truncating it.
{
    my @a = 1..*;
    is @a.shift, 1, 'shift returns the first element';
    is @a.shift, 2, 'a second shift returns the next one';
    is-deeply @a[^3], (3, 4, 5), 'the rest of the sequence follows';
    ok @a.is-lazy, 'the array is still lazy after shift';
}

{
    my @a = 1..*;
    @a.unshift(0);
    is-deeply @a[^3], (0, 1, 2), 'unshift in sink context';
    ok @a.is-lazy, 'the array is still lazy after unshift';
    ok @a.unshift(-1).is-lazy, 'unshift returns the (lazy) array';
    is @a[0], -1, 'and the element is in front';
}

{
    my @a = 1..*;
    @a.prepend(0, 1);
    is-deeply @a[^3], (0, 1, 1), 'prepend flattens its arguments';
}

{
    my @a = 1..*;
    is-deeply @a.splice(1, 2), [2, 3], 'splice returns the removed elements';
    is-deeply @a[^3], (1, 4, 5), 'and removes them from the lazy array';
    @a.splice(1, 0, 7, 8);
    is-deeply @a[^5], (1, 7, 8, 4, 5), 'splice inserts into the lazy array';
    throws-like { @a.splice(1) }, X::Cannot::Lazy,
        'a splice reaching the end of an infinite array throws';
}

{
    my @a = 1, 2, 4 ... *;
    @a[1] = 99;
    @a.shift;
    is-deeply @a[^4], (99, 4, 8, 16),
        'a front mutation keeps an element override and the generator state';
}

{
    my @a = 1, 3 ... *;
    @a.shift for ^3;
    @a.unshift(42);
    is-deeply @a[^4], (42, 7, 9, 11), 'repeated front mutations compose';
}

{
    my @a = "a" .. *;
    @a.shift for ^3;
    is-deeply @a[^2], <d e>, 'a Str-start range shifts lazily too';
}

# Callers that used to read the capped prefix.
{
    my @a = 3, 7, 9, 11;
    is-deeply @a[0, 2 ... *], (3, 9), 'an infinite sequence index stops at the end of the array';
    is-deeply @a[1 ... *], (7, 9, 11), 'a single-seed infinite index too';
    nok (1 ... *) eqv (1, 3), 'a lazy list is not eqv a finite one';
    nok (1 ... *).List eqv (1 ... 3).List, 'nor is a lazy List';
    lives-ok { (42 xx *).iterator.sink-all }, 'sinking an infinite plain-value repeat returns';
    my @b = 1..*;
    @b;
    pass 'sinking a lazy array does not force it';
}

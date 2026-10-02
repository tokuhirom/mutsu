use Test;

plan 24;

# An endpoint-less closure sequence (`1, {$_+1} ... *`) and a triangle reduce
# over an infinite source (`[\+] 1..*`) are infinite. A strict force of one
# used to hand back whatever prefix happened to be generated (33 and 200000
# elements) as the whole list (#10861). It now throws X::Cannot::Lazy, and the
# front mutators keep a lazy `@`-array over either shape lazy, as Rakudo does.

# Strict force of an infinite shape throws.
throws-like { (1, {$_ + 1} ... *).eager.elems }, X::Cannot::Lazy,
    '.eager on an endpoint-less closure sequence throws';
throws-like { ([\+] 1..*).eager.elems }, X::Cannot::Lazy,
    '.eager on a scan over an infinite range throws';

# ...but a generator that ends the sequence itself, and a scan over a finite
# lazy source, still force to their complete list.
is-deeply (1, { last if $_ >= 5; $_ + 1 } ... *).eager.List, (1, 2, 3, 4, 5),
    'a closure sequence ended by `last` forces completely';
is-deeply ([\+] gather { take $_ for 1..4 }).eager.List, (1, 3, 6, 10),
    'a scan over a finite gather forces completely';

# Bounded reads are unaffected.
is-deeply (1, 1, * + * ... *)[^10], (1, 1, 2, 3, 5, 8, 13, 21, 34, 55),
    'indexing a closure sequence';
is-deeply ([\+] 1..*)[^5], (1, 3, 6, 10, 15), 'indexing a scan';

# Front mutation of a lazy closure-sequence array keeps it lazy.
{
    my @a = 1, {$_ + 1} ... *;
    @a.shift;
    ok @a.is-lazy, 'shift keeps a closure-sequence array lazy';
    is-deeply @a[0..2], (2, 3, 4), 'shift drops the first element';
    throws-like { @a.elems }, X::Cannot::Lazy, '.elems after shift still throws';
    @a.unshift(0, -1);
    is-deeply @a[0..3], (0, -1, 2, 3), 'unshift prepends in front of the tail';
    @a.splice(1, 2, 9);
    is-deeply @a[0..3], (0, 9, 3, 4), 'a bounded splice replaces in place';
    ok @a.is-lazy, 'still lazy after unshift and splice';
}
{
    my @a = 1, {$_ + 1} ... *;
    is-deeply @a.splice(0, 2), [1, 2], 'a bounded splice answers the removed elements';
    is-deeply @a[0..2], (3, 4, 5), 'and the tail continues after them';
    ok @a.prepend(5).is-lazy, 'prepend answers the (lazy) array';
}

# A generator that reads its own history keeps reading the true history, not
# the mutated array (Rakudo's sequence iterator never sees the array it feeds).
{
    my @f = 1, 1, * + * ... *;
    @f.shift;
    @f.shift;
    is-deeply @f[0..5], (2, 3, 5, 8, 13, 21), 'Fibonacci after two shifts';
    @f.unshift(100);
    is-deeply @f[0..4], (100, 2, 3, 5, 8), 'unshift does not feed the generator';
    is-deeply @f[10], 144, 'a far element still follows the true history';
}
{
    my @a = 1, 2, { slip $^a + 10, $^b + 10 } ... *;
    @a.shift;
    is-deeply @a[0..5], (2, 11, 12, 21, 22, 31), 'a slipping generator after shift';
}

# Front mutation of a lazy scan array keeps it lazy.
{
    my @s = [\+] 1..*;
    @s.shift;
    is-deeply @s[0..2], (3, 6, 10), 'shift on a scan array';
    @s.unshift(7);
    ok @s.is-lazy, 'unshift keeps a scan array lazy';
    is-deeply @s[0..3], (7, 3, 6, 10), 'unshift on a scan array';
}
{
    my @s = [\+] 1..*;
    @s.shift for ^2;
    @s.unshift(1, 2, 3);
    is-deeply @s[0..5], (1, 2, 3, 6, 10, 15),
        'the scan keeps walking its source after the cache was rewritten';
}
{
    my @s = [\+] (1..*).map(* + 1);
    @s.shift;
    is-deeply @s[0..2], (5, 9, 14), 'shift on a scan over an infinite pipe';
}

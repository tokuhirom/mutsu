use v6.e.PREVIEW;
use Test;

plan 21;

# Range / Seq / nested-Whatever dimension resolution in reads.
{
    my @a = [[[42,666,[314]],],];
    is-deeply @a[0;0;(0,1,2)], (42,666,[314]),        'List dim read';
    is-deeply @a[0;*;(0,1,2).Seq], (42,666,[314]),    'Seq dim read flattens';
    is-deeply @a[*;0;^3], (42,666,[314]),             'Range dim read';
    is-deeply @a[0;*;(0...2)], (42,666,[314]),        'Seq-from-range dim read';
    is-deeply @a[*;*;*], (42,666,[314]),              'nested Whatever read';
}

# Multidim assignment: nested Whatever / Range / Seq distribute across leaves.
{
    my @a = [[[42,666,[314]],],];
    @a[*;*;*] = (7,8,9);
    is-deeply @a, [[[7,8,9],],],                      'nested * assignment distributes';

    @a = [[[42,666,[314]],],];
    @a[*;0;^3] = (7,8,9);
    is-deeply @a, [[[7,8,9],],],                      'Range dim assignment';

    @a = [[[42,666,[314]],],];
    @a[0;*;(0...2)] = (7,8,9);
    is-deeply @a, [[[7,8,9],],],                      'Seq dim assignment';
}

# Slice vs scalar RHS distribution semantics.
{
    my @a = [[[42,666,99],],];
    @a[0;0;0] = (7,8,9);
    is-deeply @a, [[[(7,8,9),666,99],],],             'scalar subscript stores whole list';

    @a = [[[42,666,99],],];
    @a[0;0;*-1] = (7,8,9);
    is-deeply @a, [[[42,666,7],],],                   'WhateverCode subscript is slice (first elem)';
}

# Non-Int (Str/Num/Rat) array indices coerce in both assignment and adverbs.
{
    my @a = [[[42,666,[314]],],];
    @a["0";0e0;0/1] = 999;
    is-deeply @a, [[[999,666,[314]],],],              'string/num/rat index assignment';

    @a = [[[42,666,[314]],],];
    is-deeply (@a["0";0e0;0/1]:k), (0,0,0),           ':k coerces indices';
    is-deeply (@a["0";0e0;0/1]:v), 42,                ':v with coerced indices';
    is-deeply (@a["0";0e0;0/1]:p), ((0,0,0) => 42),   ':p with coerced indices';
}

# :delete returns the deleted element decontainerized, Nil when out of range.
{
    my @a = [[[42,666,[314]],],];
    is-deeply (@a[0;0;2]:delete), (314,),             ':delete returns List (decont)';
    is-deeply @a, [[[42,666],],],                     ':delete removed the element';

    @a = [[[42,666,[314]],],];
    is-deeply (@a[0;0;3]:delete), Nil,                ':delete of missing element is Nil';
}

# Combined adverb + :delete (static and dynamic) on multidim.
{
    my @a = [[[42,666,[314]],],];
    is-deeply (@a[0;0;1]:k:delete), (0,0,1),          ':k:delete static returns key';
    is-deeply @a, [[[42,Any,[314]],],],               ':k:delete removed element';

    my $del = True;
    @a = [[[42,666,[314]],],];
    is-deeply (@a[0;0;1]:kv:delete($del)), ((0,0,1),666), ':kv:delete dynamic';

    @a = [[[42,666,[314]],],];
    is-deeply (@a[0;0;3]:k:delete), Nil,              ':k:delete of missing element is Nil';
}

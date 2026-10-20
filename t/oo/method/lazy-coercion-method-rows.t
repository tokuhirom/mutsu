use v6;
use Test;

# The lazy-list coercions (`List`, `values`, `Slip`, `Seq`, `Array`) of a
# `gather` or a scan are answered by the method table's rows and the lazy step,
# with no fix-up after the native entry (ADR-11276, slice 5).

plan 11;

is-deeply (gather { take 1; take 2 }).List, (1, 2), 'gather .List';
is-deeply (gather { take 1; take 2 }).values.List, (1, 2), 'gather .values';
is-deeply (gather { take 1; take 2 }).Slip, slip(1, 2), 'gather .Slip';
is-deeply ([\+] 1..5).List, (1, 3, 6, 10, 15), 'scan .List';
is-deeply ([\+] 1..5).Array, [1, 3, 6, 10, 15], 'scan .Array';
is-deeply ([\+] 1..5).Slip, slip(1, 3, 6, 10, 15), 'scan .Slip';
is ([\+] 1..5).Seq.elems, 5, 'scan .Seq';

# A named argument no row declares is swallowed by the implicit *%_.
is 255.fmt("%x", :zzz), 'ff', 'fmt swallows an undeclared named';
is "abc".chop(:zzz), 'ab', 'chop swallows an undeclared named';
is-deeply 10.polymod(3, :zzz), (1, 3), 'polymod swallows an undeclared named';
is 255.fmt(:zzz), '255', 'fmt with only a named';

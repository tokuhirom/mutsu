use Test;

plan 24;

# array[T] type objects do Positional / Positional[T]
ok array[int] ~~ Positional,        'array[int] type object is Positional';
ok array[int] ~~ Positional[int],   'array[int] type object is Positional[int]';
ok array[num] ~~ Positional,        'array[num] type object is Positional';
ok array[str] ~~ Positional,        'array[str] type object is Positional';

# .of on parametric type objects
ok array[int].of === int,           'array[int].of is int';
ok array[num].of === num,           'array[num].of is num';
ok Array[Int].of === Int,           'Array[Int].of is Int';
ok Positional[Int].of === Int,      'Positional[Int].of is Int';

# instances are Positional and report .of
my @a := array[int].new;
ok @a ~~ Positional,                'array[int] instance is Positional';
ok @a.of === int,                   'array[int] instance .of is int';

# out-of-bounds reads on native arrays return the element default (not the type object)
is @a[5], 0,                        'OOB read on native int array gives 0';
my @n := array[num].new;
is @n[5], 0e0,                      'OOB read on native num array gives 0e0';

# mutating map via whatever compound-assign currying
my @m = 1, 2, 3;
is-deeply @m.map(* *= 2).List, (2, 4, 6), 'mutating map (* *= 2) works';
is-deeply @m, [2, 4, 6],            'mutating map mutated the array in place';

my @mi := array[int].new(10, 20, 30);
is-deeply @mi.map(* += 5).List, (15, 25, 35), 'mutating map (* += 5) on native int array';

# shaped native arrays
my @s := array[int].new(:shape(5));
is @s.elems, 5,                     'shaped native array has 5 elems';
is @s[2], 0,                        'uninitialized shaped native int slot reads as 0';
ok @s[4]:exists,                    ':exists is true for in-range index of shaped native array';
nok @s[5]:exists,                   ':exists is false for out-of-range index';
ok @s[0]:exists,                    ':exists is true for index 0';

# :delete dies on native arrays
dies-ok { @s[0]:delete },           ':delete dies on a native array';
is (@s[0]:!delete), 0,              ':!delete returns the value without deleting';

# native num range slice-assign distributes values per element
my @rs := array[num].new;
@rs[^3] = 1e0, 2e0, 3e0;
is-deeply @rs.List, (1e0, 2e0, 3e0), 'range slice-assign distributes to native num array';

# regular whatever code still works (no regression)
is-deeply (1, 2, 3).map(* + 10).List, (11, 12, 13), 'regular whatever map still works';

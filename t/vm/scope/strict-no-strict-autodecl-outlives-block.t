use Test;

# Under `no strict` an undeclared variable is the current package's `our`
# variable, so one auto-declared inside a block is still there after the block
# (#10622). Expected values are rakudo's.

plan 12;

no strict;

{ $a1 = 5 }
is $a1, 5, 'a scalar auto-declared in a block is visible after it';
is $GLOBAL::a1, 5, '... and is the GLOBAL package variable';

{ { $a2 = 6 } }
{ is $a2, 6, 'visible from a later block, through two block exits' }

{ $a3 = 1 }
$a3++;
is $a3, 2, 'a read-modify-write after the block sees the value';

{ @a4 = 1, 2 }
@a4.push(3);
is @a4.join(','), '1,2,3', 'an array auto-declared in a block';
is @GLOBAL::a4.join(','), '1,2,3', '... is the package array';

{ %h5<x> = 1 }
%h5<y> = 2;
is %h5.sort.join(','), "x\t1,y\t2", 'a hash autovivified by an element store in a block';

{ @a6[2] = 1 }
is @a6.elems, 3, 'an array autovivified by an element store in a block';

sub reads-later { $a7 }
{ $a7 = 7 }
is reads-later(), 7, 'a routine reads the value a block stored';

if True { $a8 = 8 }
is $a8, 8, 'an `if` body';

for 1..3 { $sum9 += $_ }
is $sum9, 6, 'a `for` body';

for 1..2 -> $x10 { }
is $x10, Any, 'a pointy parameter stays the block\'s own';

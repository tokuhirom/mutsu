use Test;

# VM-native simple array mutators (push/pop/shift/unshift/append/prepend)
# on an `is Array`-backed instance's backing storage.

plan 20;

class Stack is Array {
}

# push (single + multiple args)
my $s = Stack.new;
$s.push(1);
is $s.elems, 1, 'push single grows storage';
$s.push(2, 3);
is $s.elems, 3, 'push multiple args';
is $s.join(','), '1,2,3', 'push order correct';

# pop returns the removed element and shrinks
is $s.pop, 3, 'pop returns last element';
is $s.elems, 2, 'pop shrinks storage';
is $s.join(','), '1,2', 'pop leaves the rest';

# shift returns the first element and shrinks
is $s.shift, 1, 'shift returns first element';
is $s.elems, 1, 'shift shrinks storage';
is $s.join(','), '2', 'shift leaves the rest';

# unshift prepends
$s.unshift(0);
is $s.join(','), '0,2', 'unshift prepends';

# append (one-arg flattening rule)
$s.append(9, 10);
is $s.join(','), '0,2,9,10', 'append multiple args';
$s.append((11, 12));
is $s.join(','), '0,2,9,10,11,12', 'append flattens a single list arg';

# prepend
$s.prepend(-1);
is $s.join(','), '-1,0,2,9,10,11,12', 'prepend prepends';

# pop/shift on empty storage returns an undefined Failure
my $empty = Stack.new;
my $p = $empty.pop;
nok $p.defined, 'pop on empty is undefined';
my $sh = $empty.shift;
nok $sh.defined, 'shift on empty is undefined';

# Slip flattening on push
my $t = Stack.new;
$t.push(|(4, 5, 6));
is $t.join(','), '4,5,6', 'push flattens a Slip';

# Mutations persist across multiple method calls on the same variable
my $u = Stack.new;
$u.push($_) for 1..5;
is $u.elems, 5, 'repeated push in a loop accumulates';
is $u.join(','), '1,2,3,4,5', 'loop push order';

# A subclass with extra methods keeps working
class Counter is Array {
    method total { self.elems }
}
my $c = Counter.new;
$c.push(10, 20, 30);
is $c.total, 3, 'custom method sees native-pushed elements';
is $c.join('+'), '10+20+30', 'native mutation visible to custom method';

done-testing;

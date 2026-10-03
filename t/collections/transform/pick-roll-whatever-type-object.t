use Test;

plan 8;

# The `Whatever` type object is a `Whatever` count for pick/roll, exactly
# like `*`, in both the method and the sub form.
is (^5).pick(Whatever).elems, 5, 'Range.pick(Whatever) shuffles everything';
is [1, 2, 3].pick(Whatever).sort, (1, 2, 3), 'Array.pick(Whatever) shuffles everything';
is pick(Whatever, [^5]).elems, 5, 'pick(Whatever, @list)';
is <a b>.roll(Whatever)[^4].elems, 4, 'List.roll(Whatever) is endless';
is roll(Whatever, [^5])[^3].elems, 3, 'roll(Whatever, @list) is endless';
my &chooser = &pick;
is chooser(Whatever, [^4]).elems, 4, 'a &pick code value takes Whatever';

# The sub form of roll keeps its other counts.
is roll(3, [1, 2]).elems, 3, 'roll($n, @list)';
is roll(2, 1, 2, 3).elems, 2, 'roll($n, *@values)';

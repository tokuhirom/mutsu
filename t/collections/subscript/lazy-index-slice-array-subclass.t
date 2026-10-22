use Test;

# A lazy index sequence slices an `is Array` subclass instance as it slices an
# Array: it stops at the first index past the end. `$a[1, 3 ... *]` used to die
# with "Cannot coerce an infinite lazy list to a strict list" (PDF's
# `index[1, 3 ... *]` over a `PDF::COS::Array`). Expected values are the
# `raku` (v2026.09) output.

plan 5;

class MyA is Array {}
my $a = MyA.new(214, 35, 1000, 2);
is-deeply $a[1, 3 ... *].List, (35, 2), 'odd indices up to the end';
is ([+] $a[1, 3 ... *]), 37, 'summed';
is-deeply $a[0, 2 ... *].List, (214, 1000), 'even indices';

class MyB is Array { method AT-POS($p) is rw { callsame } }
my $b = MyB.new(214, 35, 1000, 2);
is-deeply $b[1, 3 ... *].List, (35, 2), 'with an AT-POS override';
is-deeply $b[1, 3].List, (35, 2), 'a finite slice is unchanged';

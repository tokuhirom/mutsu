use Test;

# `method AT-KEY/AT-POS($k) is rw { callsame }` on an `is Hash` / `is Array`
# subclass stays assignable (PDF::COS::Tie::Hash / ::Array, from the PDF dist).

plan 6;

class H is Hash { method AT-KEY($k) is rw { my $v := callsame; $v } }
my $h = H.new;
$h<a> = 5;
is $h<a>, 5, 'bound callsame result is assignable (hash)';

class H2 is Hash { method AT-KEY($k) is rw { callsame } }
my $h2 = H2.new;
$h2<a> = 6;
is $h2<a>, 6, 'bare callsame is assignable (hash)';

class A is Array { method AT-POS($i) is rw { my $v := callsame; $v } }
my $a = A.new;
$a[0] = 5;
$a[2] = 7;
is $a[0], 5, 'bound callsame result is assignable (array)';
is $a[2], 7, 'later index too';
is $a.elems, 3, 'the array grew';

class A2 is Array { method AT-POS($i) is rw { callsame } }
my $a2 = A2.new;
$a2[1] = 3;
is $a2[1], 3, 'bare callsame is assignable (array)';

use Test;

plan 4;

# #13088: an element assignment calls a user ASSIGN-KEY / ASSIGN-POS,
# even when the class (here through a composed role) also has an
# `is rw` AT-KEY / AT-POS. Only the default ASSIGN-KEY assigns through
# AT-KEY. Reduced from PDF's `PDF::COS::Tie::Hash`.
role R {
    method AT-KEY($key) is rw { my $v := callsame; $v ~~ Int ?? $v * 10 !! $v }
    method ASSIGN-KEY($key, $val) { self.BIND-KEY($key, $val + 1) }
}
class D is Hash does R { }
my $d = D.new;
$d<a> = 4;
is $d<a>, 50, 'ASSIGN-KEY from a role runs, then AT-KEY reads';

role P {
    method AT-POS($i) is rw { my $v := callsame; $v ~~ Int ?? $v * 10 !! $v }
    method ASSIGN-POS($i, $val) { self.BIND-POS($i, $val + 1) }
}
class A is Array does P { }
my $a = A.new;
$a[0] = 4;
is $a[0], 50, 'ASSIGN-POS from a role runs, then AT-POS reads';

# Without a user ASSIGN-KEY, the assignment still goes through the rw AT-KEY.
role Q { method AT-KEY($k) is rw { my $v := callsame; $v } }
class H is Hash does Q { }
my $h = H.new;
$h<x> = 3;
is $h<x>, 3, 'an rw AT-KEY alone still takes the assignment';

class C is Hash { method ASSIGN-KEY($k, $v) { self.BIND-KEY($k, $v ~ '!') } }
my $c = C.new;
$c<k> = 'v';
is $c<k>, 'v!', 'a class-declared ASSIGN-KEY runs';

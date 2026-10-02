use v6;
use Test;

# A Bool stored into a real Hash element is itemized, and a typed `Bool $b`
# parameter must bind what the item holds. The light call path's type check
# looked at the item itself, so a routine with a return type (which takes that
# path) died with "expected Bool but got Bool (Bool::True)"
# (Template::HAML's `ser-bool`, #10638).

plan 8;

sub sb(Bool $b --> Str) { $b ?? 'T' !! 'F' }
sub si(int $i --> Int) { $i }
sub ss(Str $s --> Str) { $s }

my %h = t => True, f => False;
is sb(%h<t>), 'T', 'True from a hash element binds to Bool';
is sb(%h<f>), 'F', 'False from a hash element binds to Bool';

my %g; %g<k> = True;
is sb(%g<k>), 'T', 'element-assigned Bool binds';

my $copy = %h<t>;
is sb($copy), 'T', 'a copy of the element binds';

is si(%h<t>), 1, 'native int param takes the itemized Bool';
throws-like { ss(%h<t>) }, X::TypeCheck::Binding::Parameter,
    'a Str param still rejects the Bool';

sub rb(--> Bool) { %h<t> }
is rb(), True, 'return type check sees the Bool';

class C {
    has Bool $.e is rw = True;
    method cw(*%o) {
        my %args = e => $!e;
        for %o.kv -> $k, $v { %args{$k} = $v }
        C.new(|%args);
    }
}
is sb(C.new.cw(:x<y>).e), 'T', 'attribute rebuilt through a hash binds';

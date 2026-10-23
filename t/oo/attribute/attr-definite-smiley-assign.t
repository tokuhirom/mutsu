use v6;
use Test;

plan 6;

# A `:D` attribute refuses `Nil` and a type object on assignment, through
# both the private (`$!x`) and the public rw accessor (`.x`) forms.
class F {
    has Int:D $.n is rw = 0;
    method set-nil  { $!n = Nil }
    method set-type { $!n = Int }
}
my $f = F.new;
throws-like { $f.n = Nil },  X::TypeCheck::Assignment, 'public accessor refuses Nil';
throws-like { $f.n = Int },  X::TypeCheck::Assignment, 'public accessor refuses a type object';
throws-like { $f.set-nil },  X::TypeCheck::Assignment, 'private store refuses Nil';
throws-like { $f.set-type }, X::TypeCheck::Assignment, 'private store refuses a type object';
$f.n = 5;
is $f.n, 5, 'a defined value is still accepted';

class G { has Int $.n is rw = 3; }
my $g = G.new;
$g.n = Nil;
is $g.n.raku, 'Int', 'a plain typed attribute still resets on Nil';

done-testing;

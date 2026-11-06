use Test;

plan 5;

# `$obj.^meta = v` on an INSTANCE invocant runs a user `method ^meta($obj) is rw`
# with the instance as its first argument (a type-object invocant already did).
class P { }
class Q { }
class F {
    my %count{Mu} is default(0);
    method ^model($f) is rw { $ }
    method bump { ++%count{ self.^model } }
}

my $f = F.new;
$f.^model = P;
is $f.^model.^name, 'P', 'assigning through an rw metamethod on an instance';
is $f.bump, 1, 'the metamethod key is the type object it holds';
is $f.bump, 2, 'the counter keyed by it advances';
$f.^model = Q;
is $f.bump, 1, 'a different key starts its own count';

F.^model = P;
is F.^model.^name, 'P', 'assigning on the type object still works';

# vim: expandtab shiftwidth=4

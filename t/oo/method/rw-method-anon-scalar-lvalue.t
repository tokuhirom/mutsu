use Test;

# From the RedFactory distribution: `method ^model($f) is rw { $ }`.
plan 3;

class C { method m is rw { $ } }
my $o = C.new;
$o.m = 5;
is $o.m, 5, 'a bare $ as the tail of an is-rw method is a persistent location';
sub g is rw { $ }
g() = 3;
is g(), 3, 'same for an is-rw sub';
my $o2 = C.new;
is $o2.m, 5, 'the state cell belongs to the method, shared across invocants (as rakudo)';

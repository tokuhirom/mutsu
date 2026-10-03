use Test;

# `BIND-KEY` on an instance of a Hash subclass, called on the variable or as
# `self.BIND-KEY` inside the class, binds the key in the instance's own hash.
# It used to die with "No such method 'BIND-KEY' for invocant of type 'Hash'"
# (WriteOnceHash's STORE binds every initial pair that way).

plan 7;

class H is Hash {
    method bind-it($k, $v) { self.BIND-KEY($k, $v) }
    method assign-it($k, $v) { self.ASSIGN-KEY($k, $v) }
}

my %h is H;
%h.assign-it('a', 1);
is-deeply %h<a>, 1, 'ASSIGN-KEY from a method (baseline)';

is %h.BIND-KEY('b', 2), 2, 'BIND-KEY on the variable returns the value';
is %h<b>, 2, 'and binds the key';

%h.bind-it('c', 3);
is %h<c>, 3, 'self.BIND-KEY inside the class binds into the instance';
is %h.keys.sort.join(','), 'a,b,c', 'every key is present';
isa-ok %h, H, 'the variable still holds the subclass instance';

my $obj = H.new;
$obj.bind-it('z', 26);
is $obj<z>, 26, 'on an instance held in a scalar';

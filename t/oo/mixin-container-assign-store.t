use Test;

# Assigning to a hash or array that has a role mixed in is the container's
# STORE: the role sees it, and the container keeps the role.

plan 8;

my @seen;
my role R { method STORE(\v) { @seen.push('R'); callsame; self } }

my %h = a => 1;
%h does R;
%h = c => 3;
is-deeply @seen, ['R'], 'assignment calls the role STORE';
ok %h.^name.contains('R'), 'the role survives the assignment';
is-deeply %h.keys.List, ('c',), 'contents replaced';

sub f(%p) { %p = d => 4 }
f(%h);
ok %h.^name.contains('R'), 'assignment through a parameter keeps the role';
is %h<d>, 4, 'and reaches the caller';

for (%h,) -> %g { %g = e => 5 }
is %h<e>, 5, 'assignment through a loop parameter writes through';

my role Plain { method hi { 'hi' } }
my @a = 1, 2;
@a does Plain;
@a = 7, 8, 9;
is @a.hi, 'hi', 'an array without a role STORE keeps its role';
is-deeply @a.List, (7, 8, 9), 'and gets the new elements';

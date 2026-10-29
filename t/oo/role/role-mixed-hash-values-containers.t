use Test;

# From WriteOnceHash 0.0.9 ("no containers returned"): `.values` of a hash a
# role was mixed into, or of an `is Hash` subclass, hands out the hash's own
# element containers. An element bound with BIND-KEY is the plain value; an
# assigned one is a Scalar -- exactly as for a plain Hash.

plan 6;

role R { }

my %m does R;
%m.BIND-KEY('b', 7);
is-deeply %m.values.map({ .VAR.^name }).List, ('Int',),
    'a bound element of a role-mixed hash is not wrapped in a Scalar';

my %n does R;
%n<c> = 1;
is-deeply %n.values.map({ .VAR.^name }).List, ('Scalar',),
    'an assigned element of a role-mixed hash is a Scalar';

my %o{Any} does R;
%o.BIND-KEY(42, 'a');
is +%o.values.grep({ .VAR.^name ne .^name }), 0,
    'values of an object hash filled with BIND-KEY hold no containers';

my $loop = '';
for %m.values { $loop ~= .VAR.^name }
is $loop, 'Int', 'for over the values aliases the bound element, not a Scalar';

class H is Hash { }
my %h is H;
%h.BIND-KEY('b', 7);
%h<c> = 1;
is %h.values.map({ .VAR.^name }).sort.List, ('Int', 'Scalar'),
    'is Hash subclass: bound stays plain, assigned is a Scalar';

role S { method values { (42,) } }
my %s does S;
%s<x> = 1;
is %s.values.List, (42,), 'a role-supplied values still wins';

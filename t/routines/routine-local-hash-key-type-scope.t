use Test;

# A routine's own `my %h{K}` is frame state: its key type must not leak into
# the caller's same-named hash when the routine returns (#13139), while a
# closure still checks keys against the hash it closes over. Every
# expectation was checked against rakudo.

plan 7;

my %seen{Int};
%seen{1} = 1;
sub f($k) { my %seen{Str}; %seen{$k} = 1; %seen.elems }

is f('k'), 1, "the routine's own hash takes a Str key";
nok (try { %seen{'s'} = 1; True }), "the caller's Int-keyed hash still rejects a Str key";
lives-ok { %seen{2} = 1 }, "the caller's hash still takes an Int key";

my %o{Int};
my &c = -> $k { %o{$k} = 1 };
nok (try { c('x'); True }), 'a closure checks keys against the hash it closes over';
c(3);
is %o.keys.map(*.^name).join(','), 'Int', 'the closure stored an Int key';

sub g { my %z{Str}; -> $k { %z{$k} = 1; %z.keys.map(*.^name).join(',') } }
is g()('q'), 'Str', "a closure over a routine's own hash uses its key type";
nok (try { g()(5); True }), "that closure rejects a key of another type";

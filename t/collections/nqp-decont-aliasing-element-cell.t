use Test;
use nqp;

plan 6;

# `nqp::decont` returns the object an aliasing element cell holds, so an
# identity test against a stored sentinel succeeds (#18812, Hash::Util's
# hidden-key marker).

my \o = Mu.new;
my \sentinel = Mu.new but role { method defined { False } };
my %h;
%h<o> = o;
%h<s> = sentinel;

my %one;
%one<o> = o;
ok nqp::decont(%one.values[0]) =:= o, '.values element';
ok nqp::decont(%h.pairs.first(*.key eq 'o').value) =:= o, '.pairs value';
is nqp::eqaddr(nqp::decont(%h.pairs.first(*.key eq 's').value), sentinel), 1, 'nqp::eqaddr';
is-deeply %h.pairs.map({ .key if nqp::decont(.value) =:= sentinel }).List, ('s',),
    'the sentinel is found among the pairs';
ok nqp::decont(%h<s>) =:= sentinel, 'a subscript read';

my @a = o;
ok nqp::decont(@a.values[0]) =:= o, 'an array element through .values';

# `nqp::create` is the REPR-level allocation: it hands back a bare instance
# and never runs a `CREATE` method the class declares (rakudo prints nothing
# for the class below). mutsu used to reach its allocation through the full
# method dispatch, whose walk before its own `CREATE` arm cost ~20K
# instructions a call; it now allocates directly (#9122), and this pins that
# the direct route answers what the dispatch did.
use nqp;
use Test;

plan 6;

my @said;
class C {
    has $.x;
    method CREATE { @said.push: 'user CREATE'; nqp::create(self) }
}

my $c := nqp::create(C);
is-deeply @said, [], 'a user CREATE method is not run';
isa-ok $c, C, 'the instance is of the class';
ok $c.defined, 'and is an instance, not the type object';
nok $c.x.defined, 'its attributes are left unset';

my $b := nqp::create(IterationBuffer);
nqp::push($b, 42);
is nqp::elems($b), 1, 'an IterationBuffer comes back empty and usable';

my $u := nqp::create(Uni);
nqp::push_i($u, 65);
is nqp::strfromcodes($u), 'A', 'so does a Uni';

use Test;
use nqp;

plan 4;

# nqp::bindattr stores its value operand as it is: a Proxy becomes the
# attribute's container instead of being FETCHed on the way in (#14266,
# AttrX::Mooish's AttrProxy).
my @fetched;
my @stored;
# (inline: a Proxy returned from a routine is FETCHed by the return)
my &fetch = -> $ { @fetched.push: 1; 42 };
my &store = -> $, $v { @stored.push: $v };

class T { has $.x is rw; }
my $o = T.new;
my $bound := Proxy.new(FETCH => &fetch, STORE => &store);
nqp::bindattr($o, T, '$!x', $bound);
is @fetched.elems, 0, 'binding a Proxy variable does not FETCH';
is nqp::getattr($o, T, '$!x').VAR.^name, 'Proxy', 'the attribute holds the Proxy';

my $o2 = T.new;
my $pv;
nqp::bindattr($o2, T, '$!x', ($pv := Proxy.new(FETCH => &fetch, STORE => &store)));
is @fetched.elems, 0, 'binding the result of a `:=` expression does not FETCH';
is nqp::getattr($o2, T, '$!x').VAR.^name, 'Proxy', 'that attribute holds the Proxy too';

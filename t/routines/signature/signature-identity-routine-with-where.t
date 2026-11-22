use Test;
use nqp;

# `.signature` of a routine is one object per declaration, also when a
# parameter carries a `where` clause: code keyed by the identity of a
# `Parameter` (the `static` distribution maps `RakuAST::Parameter.meta-object`
# to `$candidate.signature.params`) must find it again (#18642). Only a
# signature literal built at run time from an empty routine
# (`:($x where &ok)`, #18570) gets a signature per closure value.
plan 7;

multi sub f(Int $x where * > 0) { 1 }
multi sub f(Int $x) { "s" }
sub g(Int $x where * > 0, $y = $x + 1) { 1 }
my &c = -> Int $x where * > 0 { $x };

for |&f.candidates, &g, &c -> $code {
    is nqp::objectid($code.signature), nqp::objectid($code.signature),
        "{$code.signature.raku}: one Signature object";
}
is nqp::objectid(&g.signature.params[0]), nqp::objectid(&g.signature.params[0]),
    'one Parameter object';

# Smartmatching a closure's signature never runs the closure's body.
my $ran = False;
my &body = sub (Int $x where * > 0) { $ran = True; 1 };
ok \(5) ~~ &body.signature, 'the capture matches';
nok $ran, '...without running the routine';

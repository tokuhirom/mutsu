use Test;

# A user `trait_mod:<is>(Parameter ...)` is applied to the parameters of a
# `method` as it is to a sub's, and the role it mixes in is visible through
# `.signature.params` (#14682). Expected values are the `raku` output.

plan 8;

my role HasT { has Str $.mcp-type is required }
multi trait_mod:<is>(Parameter $p, Str :$mcp-type!) { $p does HasT($mcp-type) }

sub f(Int :$val! is mcp-type<number>) { }
is &f.signature.params[0].?mcp-type, 'number', 'a sub parameter (control)';

class C {
    method m(Int :$val! is mcp-type<number>, Str :$plain) { }
    multi method n(Int $x is mcp-type<integer>) { }
    submethod s(Bool :$flag is mcp-type<boolean>) { }
}

my %by-name = C.^lookup('m').signature.params.map({ ($_.name // '') => $_ });
is %by-name<$val>.?mcp-type, 'number', 'a named method parameter';
ok !%by-name<$plain>.?mcp-type.defined, 'an untouched sibling has no such attribute';
is C.^lookup('n').candidates[0].signature.params.grep(*.name eq '$x')[0].?mcp-type, 'integer',
    'a multi method parameter';
is C.^lookup('s').signature.params.grep(*.name eq '$flag')[0].?mcp-type, 'boolean',
    'a submethod parameter';
ok C.new.can('m'), 'the class still composes';
lives-ok { C.new.m(:val(3)) }, 'and the method is callable';

dies-ok { EVAL 'class D { method m(Int :$v is nonesuch-trait) { } }' },
    'an unknown parameter trait on a method is still refused';

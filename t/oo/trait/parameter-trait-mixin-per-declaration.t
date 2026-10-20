use Test;

# A `trait_mod:<is>(Parameter ...)` mutates the declared parameter's own
# `Parameter` object, which `.signature.params` hands out afterwards: the role
# it mixed in and the role's attribute value both stay, per declaration
# (MCP::Server's `is mcp-type<number>`, #13528).

plan 9;

multi trait_mod:<is>(Parameter $param, Str :$mcp-type!) {
    my role HasMCPType { has Str $.mcp-type is required }
    $param does HasMCPType($mcp-type)
}

sub f(Int :$n is mcp-type<number>, Str :$s is mcp-type<string>, Bool :$b) { }
my @p = &f.signature.params;
is @p[0].mcp-type, 'number', 'the trait\'s value stays on the parameter';
is @p[1].mcp-type, 'string', 'another declaration keeps its own value';
nok @p[2].can('mcp-type'), 'a parameter without the trait is untouched';
is @p[0].name, '$n', 'the mutated object is still that parameter';
ok &f.signature.params[0] === &f.signature.params[0], 'one Parameter object per declaration';

sub g(Int $x is mcp-type<integer>) { }
is &g.signature.params[0].mcp-type, 'integer', 'a positional parameter too';
is &f.signature.params[0].mcp-type, 'number', 'an earlier declaration is unaffected';

# `$obj does R(value)` on an object initializes R's one public attribute in
# place, so every holder of the object sees it.
role Tagged { has $.tag }
class Box { }
my $box = Box.new;
my $alias = $box;
$box does Tagged('red');
is $alias.tag, 'red', 'an initialized mixin reaches every holder';

# A mixin type that names a lexical role displays the role's own name.
sub mix($o) { my role Local { }; $o does Local }
my $o = Box.new;
mix($o);
is $o.^name, 'Box+{Local}', 'a lexical role in a mixin type name';

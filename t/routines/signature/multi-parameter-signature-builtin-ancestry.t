use Test;

plan 9;

# `Parameter` and `Signature` are ordinary `Any` subclasses: a candidate typed
# `Mu` or `Any` is two steps wide of them, never "unrelated", so it cannot
# out-rank a nominally exact candidate whose `where` clause failed.

sub sample(Str :$msg!) { }
my $par = &sample.signature.params[0];
my $sig = &sample.signature;

is Parameter.^mro.map(*.^name).join(","), "Parameter,Any,Mu", 'Parameter MRO';
is Signature.^mro.map(*.^name).join(","), "Signature,Any,Mu", 'Signature MRO';

multi pw($_ where { False }) { "where" }
multi pw(Parameter $p) { "exact" }
multi pw(Mu $m) { "mu" }
is pw($par), "exact", 'Parameter: exact candidate beats Mu after a failed where';

multi sw(Signature $_ where { False }) { "where" }
multi sw(Signature $p) { "exact" }
multi sw(Mu $m) { "mu" }
is sw($sig), "exact", 'Signature: exact candidate beats Mu after a failed where';

multi tw(Parameter $_ where { False }) { "where" }
multi tw(Parameter $p) { "exact" }
multi tw(Any $m) { "any" }
is tw($par), "exact", 'Parameter: exact candidate beats Any';

multi ok(Parameter $_ where { True }) { "where" }
multi ok(Parameter $p) { "exact" }
multi ok(Mu $m) { "mu" }
is ok($par), "where", 'a passing where on the exact type still wins';

# The MCP::Server DSL shape: a Parameter-typed multi family with a Mu catch-all.
my %type-map = %(Any => "string", Str => "string", Int => "integer");
multi type-map(Parameter $_ where *.?mcp-type) { .mcp-type }
multi type-map(Parameter $par) { type-map $par.type }
multi type-map(Mu $_ where { %type-map{.^name}:!exists }) { die "no map for {.^name}" }
multi type-map(Mu $_) { %type-map{.^name} }
is type-map($par), "string", 'a Parameter maps through its type';

sub two(Int :$n) { }
is type-map(&two.signature.params[0]), "integer", 'second parameter';
is-deeply &sample.signature.params.map(&type-map).List, ("string",), 'map over the parameter list';

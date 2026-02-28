use Test;

plan 3;

my $x = 42;

is $$x, 42, '$$scalar parses and evaluates';
is($$x, 42, '$$scalar works in call argument position');
is($$$x, 42, 'repeated scalar dereference syntax parses');

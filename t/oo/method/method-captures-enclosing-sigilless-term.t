use Test;

# A method closes over a sigilless binding (`my \T`) of the scope that
# declares its class, as it does over a `my $t`: called after that scope
# exited, it still reads the binding. mutsu's declaration-time method capture
# dropped the binding's term key (`\T`), so the method read `(Any)`.

plan 4;

my $obj = do { my \T = 'sigilless'; my $t = 'scalar'; class MCES { method term { T }; method var { $t } }; MCES.new };
is $obj.term, 'sigilless', 'a sigilless term outlives its block';
is $obj.var, 'scalar', 'as a scalar does';

my $anon = do { my \U = 'anon'; my $c = class { method u { U } }; $c.new };
is $anon.u, 'anon', 'in an anonymous class too';

sub mk { my \V = 'routine'; class MCESR { method v { V } }; MCESR }
is mk().v, 'routine', 'and from a routine body';

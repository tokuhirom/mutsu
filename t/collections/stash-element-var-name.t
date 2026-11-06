use Test;

# `.VAR.name` of a package stash symbol is the symbol's declared name, also
# through `Pkg::<$v>` and through a variable bound to the stash cell (#16923).

plan 7;

module TI { our $v1 = 42; our @a = 1, 2; our %h = a => 1; }
my $items = TI.WHO;

is $items<$v1>.VAR.name, '$v1', 'stash element scalar';
is $items<@a>.VAR.name, '@a', 'stash element array';
is $items<%h>.VAR.name, '%h', 'stash element hash';
is TI::<$v1>.VAR.name, '$v1', 'Pkg::<$v>';
my $c := $items<$v1>;
is $c.VAR.name, '$v1', 'variable bound to the stash cell keeps its name';
$c = 5;
is TI::<$v1>, 5, 'bound variable writes through';
our $o = 2;
my $e := $o;
is $e.VAR.name, '$o', 'variable bound to an our scalar';

use lib 't/lib';
use Test;

# A module's top-level `constant` initialised from a class the module declared
# earlier in its own file must resolve even when another `unit module` that
# shares the first `::`-segment was `use`d before (#16506).

plan 3;

use Ex16506::Triv;
need Ex16506::Min;

is Ex16506::Min.sym.^name, 'Ex16506::Min::Sym', 'the constant holds the module\'s own class';
is Ex16506::Min::Sym.tag, 'sym', 'the class is usable';
is Ex16506::Min.new.sym.tag, 'sym', 'through an instance method';

done-testing;

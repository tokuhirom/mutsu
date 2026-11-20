use lib 't/lib-temp-export-hook';
use Test;

# `temp Type.method = value` and `temp Type.^meta = value` in a unit that
# imported through a `sub EXPORT` hook. Such a unit parses every bareword as
# an imported term, and the `temp` statement did not recognize the type
# invocant in that shape, so `temp` was left as a call to an unknown function
# (Red's t/023-metamodel-model, `temp Bla.^table = "temp_bla"`; #18377).
# Expected values are the `raku` output.

plan 7;

use TempExportHook;

is hooked(), 'hooked', 'the hook routine is imported';

class Counter {
    my $label = 'orig';
    method label() is rw { $label }
}

{
    temp Counter.label = 'temp';
    is Counter.label, 'temp', 'temp Type.method = value assigns inside the block';
}
is Counter.label, 'orig', 'temp Type.method is restored at block exit';

class TableHOW is Metamodel::ClassHOW {
    has $!table;
    method table(Mu \type) is rw { $!table //= 'bla' }
}
my \Bla = TableHOW.new_type(:name<Bla>);
Bla.^compose;

{
    temp Bla.^table = 'temp_bla';
    is Bla.^table, 'temp_bla', 'temp Type.^meta = value assigns inside the block';
}
is Bla.^table, 'bla', 'temp Type.^meta is restored at block exit';

my $x = 1;
{
    temp $x = 2;
    is $x, 2, 'temp $var still works in the same unit';
}
is $x, 1, 'temp $var is restored';

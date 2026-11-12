use v6;
use Test;

# A class-scoped accessor (`my $.x`) answers on the type object however late
# the class that declares it is compiled -- even after calls that asked the
# same name of classes without one. Verified against Rakudo.

plan 5;

class Plain { method other() { 'method' } }

is Plain.other, 'method', 'a class without a class-level attribute answers by its method';

my \declared = EVAL q:to/END/;
    class Later {
        my $.other = 'class-level';
    }
    Later
    END

is declared.other, 'class-level', 'a class-level accessor declared later answers';
is Plain.other, 'method', 'a class without one is unaffected';

role HasCount { my $.count = 3 }
class Counted does HasCount { }
is Counted.count, 3, 'a class-level attribute composed from a role answers';
is Plain.other, 'method', 'and the plain class still uses its method';

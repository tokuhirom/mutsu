use Test;

# A class-body statement that writes an outer lexical writes THAT lexical — a
# lexical always wins over a same-named package variable (#11086). mutsu used
# to compile the write package-qualified (`$F::z`) and copy the result back
# over the outer binding by value, which cut a method's capture (a shared cell)
# off the outer variable.

plan 8;

my $z = 1;
class Reader { method m { $z } }
class Writer { $z = 4 }
is Reader.new.m, 4, 'a method capture sees a later class-body write';
is $z, 4, '... and so does the outer scope';

class Updater { $z++; $z ~= 'x' }
is Reader.new.m, '5x', 'read-modify-write in a class body goes through the cell';

my @a;
class ArrayWriter { @a.push(1); @a = 5, 6 }
is-deeply @a, [5, 6], 'class-body writes reach an outer array';

# A body `our` of the same name is a package variable there.
my $o = 1;
class OurShadow { our $o = 9; method m { $o } }
is OurShadow.new.m, 9, 'a body `our` shadows the outer lexical inside the class';
is $o, 1, '... and leaves the outer lexical alone';
is $OurShadow::o, 9, '... and is the package variable';

# Declared inside a routine.
sub in-routine {
    my $q = 1;
    my class InRoutine { $q = 3; method m { $q } }
    $q++;
    InRoutine.new.m ~ ',' ~ $q
}
is in-routine(), '4,4', 'a routine-scoped class body writes the routine lexical';

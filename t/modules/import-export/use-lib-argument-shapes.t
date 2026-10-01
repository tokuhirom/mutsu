use Test;

plan 7;

# `use lib` runs at BEGIN time, so every path its argument names must be on the
# search path while the rest of the unit is still being parsed: the modules
# below export classes that later signatures name, which only parse when the
# import is known at parse time. The argument may be a list, a parenthesised
# list, a parenthesised path inside a list, or a path-method chain — mutsu's
# parse-time decoder used to unpack only one bare list level and skipped
# anything parenthesised. (A list *nested* in the list is not a path spec:
# rakudo rejects `use lib 'x', ('a', 'b')`.)

use lib (('t/lib/UseLibShapes/a'), 't/lib/UseLibShapes/b');
use UseLibShapeA;
use UseLibShapeB;

sub take-a(ShapeA $a) { $a.v }
sub take-b(ShapeB $b) { $b.v }
is take-a(ShapeA.new), 'A', 'parenthesised path inside a parenthesised `use lib` list';
is take-b(ShapeB.new), 'B', 'second path of a parenthesised `use lib` list';

use lib $?FILE.IO.parent(3).add('lib/UseLibShapes/c');
use UseLibShapeC;

sub take-c(ShapeC $c) { $c.v }
is take-c(ShapeC.new), 'C', '`$?FILE` path-method chain';

use lib ($?FILE.IO.parent(3)).add('lib/UseLibShapes/b');
use UseLibShapeB;

sub take-b2(ShapeB $b) { $b.v }
is take-b2(ShapeB.new), 'B', 'parenthesised link in a `$?FILE` path-method chain';

is EVAL(q:to/CODE/), 'AB', 'a parenthesised list, in EVAL';
    use lib ('t/lib/UseLibShapes/a', 't/lib/UseLibShapes/b');
    use UseLibShapeA;
    use UseLibShapeB;
    sub f(ShapeA $a, ShapeB $b) { $a.v ~ $b.v }
    f(ShapeA.new, ShapeB.new)
    CODE

is EVAL(q:to/CODE/), 'C', 'a parenthesised single path, in EVAL';
    use lib ('t/lib/UseLibShapes/c');
    use UseLibShapeC;
    sub f(ShapeC $c) { $c.v }
    f(ShapeC.new)
    CODE

is EVAL(q:to/CODE/), 'A', 'an angle-word list, in EVAL';
    use lib <t/lib/UseLibShapes/x t/lib/UseLibShapes/a>;
    use UseLibShapeA;
    sub f(ShapeA $a) { $a.v }
    f(ShapeA.new)
    CODE

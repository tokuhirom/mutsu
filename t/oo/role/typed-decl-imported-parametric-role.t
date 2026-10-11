use Test;
use lib 't/lib';
use ParamRoleExporter;

# `my R[T] $x .= method(...)` where R is a parameterized role imported from a
# module: the declared type names the exporter's R (found via
# NativeHelpers::Blob's `my LinearArray[Point3D] $lum .= new-from-pointer`).
plan 6;

class P { }

my Box[P] $a .= from-size(:10size);
is $a.n, 10, '.= with a named argument on an imported parameterized role';

my Box[P] $b .= new(4);
is $b.n, 4, '.= new on an imported parameterized role';

my Box[P] $c;
is $c.^name, 'ParamRoleExporter::Box[P]', 'the type object carries the exporter-qualified name';
ok $c ~~ Box[P], 'it matches the same parameterization';
nok $c.defined, 'and is undefined';

my Box[P] $d = Box[P].new(7);
is $d.n, 7, 'an explicit construction agrees';

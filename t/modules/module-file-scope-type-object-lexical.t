use Test;
use lib 't/lib';
use UnitLexTypeObject;
use BlockModuleMyTypeObject;

plan 19;

# #10379: a file-scope scalar of a `unit module` that holds a type object
# (`my Int $x;`, `my IO::Handle $fh;`, untyped `my $x;`, `my $x = Int`) read
# back as `Nil` from a plain routine of the module. The loader mistook such a
# variable for a leaked type-name binding and removed it from the loading
# scope's env, so the module's own lexical store was seeded from a missing key.

# `unit module`: `my` scalars.
ok read-handle() === IO::Handle, 'my IO::Handle $h keeps its type object';
ok read-handle().WHAT === IO::Handle, '... and .WHAT reports it';
ok read-handle() ~~ IO::Handle, '... and it still smartmatches its type';
ok !read-handle().defined, '... and stays undefined';
is read-handle().raku, 'IO::Handle', '... and .raku prints the type name';

ok read-typed-ro() === Int, 'my Int $x keeps the Int type object';
ok read-typed() === Int, '... also when another routine of the module writes it';
ok read-untyped() === Any, 'an untyped my $x is Any, not Nil';
is read-untyped().raku, 'Any', '... and .raku prints Any';
ok read-assigned-type() === Str, 'my $x = Str keeps the assigned type object';

my ($a, $b) = read-destructured();
ok $a === Any && $b === Any, 'my ($a, $b) list declaration gives Any for each';

# Unchanged behaviour: defined values and writes.
is read-defined(), 42, 'a defined file-scope scalar is still readable';
set-typed(7);
is read-typed(), 7, 'a write from another routine is visible afterwards';

# The module's lexical must not be reachable from the loading scope.
my $typed = 'mine';
is $typed, 'mine', 'the importer keeps its own same-named lexical';
is read-typed(), 7, '... without disturbing the module variable';
dies-ok { EVAL '$handle' }, 'a module-private $handle is not visible to the importer';

# The braced form is unaffected and does not leak its lexicals either.
ok read-block-typed() === Int, 'my Int $x in a braced module keeps the type object';
ok read-block-untyped() === Any, 'an untyped my $x in a braced module is Any';
dies-ok { EVAL '$block-typed' }, 'a braced module lexical is not visible to the importer';

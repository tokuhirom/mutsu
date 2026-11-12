use Test;
use lib 't/lib-operator-import-loading-block';

# A module first loaded inside a block keeps the operators it imported for its
# own code after the block exits: the block rolls back only its own lexical
# operator declarations (#17513: Red's SQLiteMaster, loaded lazily from inside
# a block, compared with the core `eq` on every later call, so
# `.grep(*.is-table)` compiled to `WHERE 0`).

plan 5;

# Red loads its driver this way: `require ::($driver-name)` in a routine.
sub load(Str $name) { require ::($name); ::($name) }

{
    my $model = load('OpModel');
    is $model.is-table, "type = 'table'", 'the imported eq inside the loading block';
}
{
    my $model = load('OpModel');
    is $model.is-table, "type = 'table'", 'the imported eq after the loading block exited';
}
is load('OpModel').is-table, "type = 'table'", 'and from the mainline';

{
    sub infix:<+>($a, $b) { 'block-local' }
    is 1 + 2, 'block-local', 'a block-local operator applies inside its block';
}
is 1 + 2, 3, 'and is rolled back when the block exits';

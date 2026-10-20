use Test;
use lib $*PROGRAM.parent(3).add('lib');

# A block-scoped `use` binds the types it imports in the block's pad
# (ADR-8751): inside the block, `UibtThing` in a type position names the
# module's class even where the program declares its own `UibtThing`, and a
# routine or closure taken out of the block keeps it. mutsu resolved such a
# type position by name at run time, against the program's own type.

plan 14;

class UibtThing { method who { 'program thing' } }
role UibtRole { method tag { 'program role' } }

my $module-thing = do { use UseInBlockTypePositions; UibtThing.new };
is $module-thing.who, 'module thing', 'the block term is the import';

my &takes = do {
    use UseInBlockTypePositions;
    sub (UibtThing $x) { $x.who }
};
is takes($module-thing), 'module thing', 'a parameter typed in the block takes the import';
nok (try takes(UibtThing.new)), '...and rejects the program type of the same name';

my &gives = do {
    use UseInBlockTypePositions;
    sub (--> UibtThing) { UibtThing.new }
};
is gives().who, 'module thing', 'a return type in the block is the import';

my &typed = do {
    use UseInBlockTypePositions;
    sub ($v) { my UibtThing $y = $v; $y.who }
};
is typed($module-thing), 'module thing', 'a typed variable in the block takes the import';
nok (try typed(UibtThing.new)), '...and rejects the program type';

my &is-thing = do {
    use UseInBlockTypePositions;
    -> $x { $x ~~ UibtThing }
};
ok is-thing($module-thing), 'a smartmatch in a closure taken out of the block matches the import';
nok is-thing(UibtThing.new), '...and not the program type';

my $module-role = do { use UseInBlockTypePositions; class :: does UibtRole { }.new };
my &takes-role = do {
    use UseInBlockTypePositions;
    sub (UibtRole $r) { $r.tag }
};
is takes-role($module-role), 'module role', 'an imported role types a parameter';
nok (try takes-role(class :: does UibtRole { }.new)), '...and rejects a doer of the program role';

# Seen before the `use` in the same block: the pad entry exists from the
# start of the block.
my &early = do {
    my &f = sub (UibtThing $x) { $x.who };
    use UseInBlockTypePositions;
    &f
};
is early($module-thing), 'module thing', 'a routine written before the use sees the import';

# A tag the `use` does not name is not imported.
my &untagged = do {
    use UseInBlockTypePositions;
    sub (UibtThing $x) { 'ok' }
};
ok untagged($module-thing), 'the default import binds the default tag';

# Outside the block the program's types are unchanged.
sub outer-takes(UibtThing $x) { $x.who }
is outer-takes(UibtThing.new), 'program thing', 'a routine outside the block takes the program type';
nok (try outer-takes($module-thing)), '...and rejects the import';

use Test;
use lib $*PROGRAM.parent(3).add('lib');

# A block-scoped `use` binds an imported type as a term of the block's pad
# (ADR-8751): `UibtThing.new` in the block, in a closure taken out of it, and
# in a method of a class the block declares names the module's class, and so
# does a parent or role of such a class. mutsu bound the type through an env
# alias, which a method run after the block exited no longer saw: it fell
# back to the program's own `UibtThing`.

plan 9;

class UibtThing { method who { 'program thing' } }
role UibtRole { method tag { 'program role' } }

my $obj = do {
    use UseInBlockTypePositions;
    class UibtUser { method make { UibtThing.new.who } };
    UibtUser.new
};
is $obj.make, 'module thing', 'a method of a class declared in the block keeps the imported type';

my $sub = do { use UseInBlockTypePositions; class UibtChild is UibtThing { }; UibtChild.new };
is $sub.who, 'module thing', 'an imported parent class';

my $doer = do { use UseInBlockTypePositions; class UibtDoer does UibtRole { }; UibtDoer.new };
is $doer.tag, 'module role', 'an imported role';

my $mixed = do { use UseInBlockTypePositions; UibtThing.new but UibtRole };
is $mixed.tag, 'module role', 'an imported role mixed in with but';

my &mk = do { use UseInBlockTypePositions; -> { UibtThing.new.who } };
is mk(), 'module thing', 'a closure taken out of the block keeps the type term';

{
    use UseInBlockTypePositions;
    is UibtThing.^name, 'UseInBlockTypePositions::UibtThing', 'the term in the block';
    is EVAL('UibtThing').^name, 'UseInBlockTypePositions::UibtThing', 'an EVAL in the block sees it';
}

sub callee { UibtThing.who }
{
    use UseInBlockTypePositions;
    is callee(), 'program thing', 'a routine written outside the block takes the program type';
}

is UibtThing.^name, 'UibtThing', 'after the block the program type is back';

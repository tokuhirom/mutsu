use lib $?FILE.IO.parent(2).add('lib').Str;
use Test;

# A module's own top-level `use` binds the variables and types it imports in
# the module's mainline pad (ADR-8751): the module's routines and closures see
# its imports, the importer's same-named lexicals do not get in the way, and
# nothing leaks into the importer (#14140). Checked against rakudo.

plan 10;

my $mub-value = 'caller';
my @mub-list = <caller>;
my $mub-ro = 'caller';
use ModuleUnitBind::Writer;
use ModuleUnitBind::Reader;
use ModuleUnitBind::Readonly;

is writer-reads(), 'written/a,b', "a module routine reads its use's variables, not the caller's";
is reader-reads(), 'written', "the module's write went to the exported variable itself";
my &c = writer-closure();
{
    my $mub-value = 'shadow';
    is c(), 'written/hi', 'a closure the module returns keeps its imports under a shadowing caller';
}
is writer-typed(ModuleUnitBind::Vars::MubClass.new), 'hi',
    'an imported type works in a module routine signature';
is $mub-value, 'caller', "the caller's same-named scalar is untouched";
is @mub-list.join(','), 'caller', "the caller's same-named array is untouched";
is (try EVAL 'MubClass') // 'not visible', 'not visible', 'the imported class does not leak to the caller';
is (try EVAL '$mub-value'), 'caller', 'EVAL in the caller sees its own variable';

# The module's own `my $x := 'v'` is readonly in the module only.
is ro-reads(), 'bound', "a module routine reads the module's readonly binding";
$mub-ro = 'reassigned';
is $mub-ro, 'reassigned', "the caller's same-named variable stays assignable";

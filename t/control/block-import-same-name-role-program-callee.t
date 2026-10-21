use Test;
use lib 't/lib';

# A top-level routine called from a `use` block that imports a role under the
# name of the program's own `my role` composes the program's role when it
# declares a class `does` it (ADR-8751).
plan 4;

my role SameNameRole { method who { "program" } }
my class UsesIt does SameNameRole { }
sub callee { UsesIt.new.who ~ '/' ~ SameNameRole.^name }
sub declaring-callee { my class Local does SameNameRole { }; Local.new.who }
{
    use BlockImportSameNameRole;
    is callee(), 'program/SameNameRole', 'a callee sees the program role';
    is declaring-callee(), 'program', 'a class declared in the callee composes it';
    is (class :: does SameNameRole { }).new.who, 'module', 'in the block the import wins';
}
is declaring-callee(), 'program', 'and after the block';

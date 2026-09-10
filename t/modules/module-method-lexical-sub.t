use Test;
use lib 't/lib';
use ModuleMethodLexical;

sub helper($value) { "importer:$value" }
package Unrelated {
    our sub helper($value) { "unrelated:$value" }
}

plan 4;

my $target = ModuleMethodLexicalTarget.new;
is $target.declared, 'module:5', 'a declared method sees its compunit routine';
is $target.added, 'module:5', 'an added method sees its defining compunit routine';
is Unrelated::ModuleMethodLexicalTarget.new.declared, 'module:6',
    'the compunit routine precedes unrelated enclosing packages';
is ModuleMethodLexical::helper(5), 'module:5', 'qualified lookup remains available';

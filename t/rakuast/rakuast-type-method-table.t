use Test;
use experimental :rakuast;

plan 10;

my %int-methods = RakuAST::IntLiteral.^method_table;
is %int-methods.keys.sort.join(','), 'new',
    'literal method table exposes the model API';
is %int-methods<new>.name, 'new', 'constructor entry is a Method object';
is RakuAST::Literal.^method_table<value>.name, 'value',
    'the declaring parent has the accessor Method object';
is %int-methods.values.map(*.^name).unique.join(','), 'Method',
    'method table values use ordinary Method introspection objects';

my %apply-methods = RakuAST::ApplyInfix.^method_table;
is %apply-methods.keys.sort.join(','), 'add-colonpair,colonpairs,infix,left,new,operands,operator,return-type,right',
    'multi-field node exposes constructor, mutator and accessors';

is RakuAST::Name.^method_table.keys.sort.join(','),
    'base-name,canonicalize,colonpairs,first-part,from-identifier,from-identifier-parts,has-colonpairs,is-anonymous,is-empty,is-global-lookup,is-identifier,is-indirect-lookup,is-installable,is-multi-part,is-package-lookup,is-pseudo-package,is-simple,last-part,new,parts,root-part,simple-identifier,without-colonpairs,without-first-part',
    'named constructor appears in the method table';
is RakuAST::StatementList.^method_table.keys.sort.join(','), 'add-statement,code-statements,new,statements,unshift-statement',
    'mutable model class exposes construction, mutation, and its accessor';
is RakuAST::Assignment.^method_table.keys.sort.join(','), 'new',
    'Assignment inherits its operator accessor from Infix';

my $node = RakuAST::IntLiteral.new(42);
is $node.^method_table.keys.sort.join(','), 'new',
    'node values share their type object method table';
is $node.^lookup('value').name, 'value',
    'node lookup retains usable inherited accessor metadata';

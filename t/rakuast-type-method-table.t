use Test;
use experimental :rakuast;

plan 10;

my %int-methods = RakuAST::IntLiteral.^method_table;
is %int-methods.keys.sort.join(','), 'new,value',
    'literal method table exposes the model API';
is %int-methods<new>.name, 'new', 'constructor entry is a Method object';
is %int-methods<value>.name, 'value', 'accessor entry is a Method object';
is %int-methods.values.map(*.^name).unique.join(','), 'Method',
    'method table values use ordinary Method introspection objects';

my %apply-methods = RakuAST::ApplyInfix.^method_table;
is %apply-methods.keys.sort.join(','), 'infix,left,new,right',
    'multi-field node exposes constructor and accessors';

is RakuAST::Name.^method_table.keys.sort.join(','), 'from-identifier',
    'named constructor appears in the method table';
is RakuAST::StatementList.^method_table.keys.sort.join(','), 'add-statement,new,statements',
    'mutable model class exposes construction, mutation, and its accessor';
is RakuAST::Assignment.^method_table.elems, 0,
    'class without an implemented model API has an empty table';

my $node = RakuAST::IntLiteral.new(42);
is $node.^method_table.keys.sort.join(','), 'new,value',
    'node values share their type object method table';
is $node.^method_table<value>.name, 'value',
    'node value method table contains usable metadata';

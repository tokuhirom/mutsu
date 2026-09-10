use Test;
use experimental :rakuast;

plan 12;

my @int-methods = RakuAST::IntLiteral.^methods(:local).map(*.name);
ok @int-methods.grep(* eq 'new'), 'literal type object exposes its constructor';
ok @int-methods.grep(* eq 'value'), 'literal type object exposes its value accessor';
is @int-methods.sort.join(','), 'new,value', 'literal local method list is model-only';

my $int = RakuAST::IntLiteral.new(42);
is $int.^methods(:local).map(*.name).sort.join(','), 'new,value',
    'node value uses the same local method metadata';

my @infix-methods = RakuAST::ApplyInfix.^methods(:local).map(*.name);
ok @infix-methods.grep(* eq 'new'), 'multi-field node exposes new';
ok @infix-methods.grep(* eq 'left'), 'multi-field node exposes left';
ok @infix-methods.grep(* eq 'infix'), 'multi-field node exposes infix';
ok @infix-methods.grep(* eq 'right'), 'multi-field node exposes right';

is RakuAST::Name.^methods(:local).map(*.name).sort.join(','),
    'from-identifier', 'Name exposes its supported named constructor';

is RakuAST::StatementList.^methods(:local).map(*.name).sort.join(','),
    'add-statement,new,statements',
    'StatementList exposes construction, mutation, and its read accessor';

is RakuAST::Statement::Expression.^methods(:local).map(*.name).sort.join(','),
    'expression,loop-modifier,new',
    'statement wrapper exposes constructor and accessors';

is RakuAST::Postfix.^methods(:local).map(*.name).sort.join(','),
    'new,operator', 'Postfix exposes its named field as an accessor';

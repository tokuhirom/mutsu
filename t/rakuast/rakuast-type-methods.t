use Test;
use experimental :rakuast;

plan 12;

my @int-methods = RakuAST::IntLiteral.^methods(:local).map(*.name);
ok @int-methods.grep(* eq 'new'), 'literal type object exposes its constructor';
ok RakuAST::Literal.^methods(:local).map(*.name).grep(* eq 'value'),
    'Literal declares the inherited value accessor';
is @int-methods.sort.join(','), 'new', 'literal local methods retain declaring owners';

my $int = RakuAST::IntLiteral.new(42);
is $int.^methods(:local).map(*.name).sort.join(','), 'new',
    'node value uses the same local method metadata';

my @infix-methods = RakuAST::ApplyInfix.^methods(:local).map(*.name);
ok @infix-methods.grep(* eq 'new'), 'multi-field node exposes new';
ok @infix-methods.grep(* eq 'left'), 'multi-field node exposes left';
ok @infix-methods.grep(* eq 'infix'), 'multi-field node exposes infix';
ok @infix-methods.grep(* eq 'right'), 'multi-field node exposes right';

is RakuAST::Name.^methods(:local).map(*.name).sort.join(','),
    'base-name,canonicalize,colonpairs,first-part,from-identifier,from-identifier-parts,has-colonpairs,is-anonymous,is-empty,is-global-lookup,is-identifier,is-indirect-lookup,is-installable,is-multi-part,is-package-lookup,is-pseudo-package,is-simple,last-part,new,parts,root-part,simple-identifier,without-colonpairs,without-first-part',
    'Name exposes its supported constructors and accessor';

is RakuAST::StatementList.^methods(:local).map(*.name).sort.join(','),
    'add-statement,code-statements,new,statements,unshift-statement',
    'StatementList exposes construction, mutation, and its read accessor';

is RakuAST::Statement::Expression.^methods(:local).map(*.name).sort.join(','),
    'condition-modifier,expression,is-block-statement,loop-modifier,new,set-expression',
    'statement wrapper exposes constructor and accessors';

is RakuAST::Postfix.^methods(:local).map(*.name).sort.join(','),
    'is-resolved,new,operator,resolution', 'Postfix exposes its own accessors';

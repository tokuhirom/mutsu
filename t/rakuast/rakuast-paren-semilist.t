use Test;

# `(1; 2)` is `Circumfix::Parentheses(SemiList)` with one statement per
# semicolon section, where `(1, 2)` has a single statement (#14820).

plan 10;

sub semilist($source) {
    $source.AST.statements[0].expression.initializer.expression.semilist
}

my $two = semilist('my $x = ( 1 ; 2 ; );');
is $two.^name, 'RakuAST::SemiList', 'parentheses hold a SemiList';
is $two.statements.elems, 2, 'two sections are two statements';
is-deeply $two.statements.map(*.^name).List,
    ('RakuAST::Statement::Expression', 'RakuAST::Statement::Expression'),
    'each is a Statement::Expression';

my $mixed = semilist('my $x = (1,2;3);');
is $mixed.statements.elems, 2, 'a comma list and a lone item are two statements';
is $mixed.statements[0].expression.^name, 'RakuAST::ApplyListInfix', 'the first holds the comma list';

is semilist('my $x = (1,2,3;);').statements.elems, 1, 'a trailing `;` adds no statement';
is semilist('my $x = (1,2);').statements.elems, 1, 'a plain comma list is one statement';

is-deeply EVAL('(1,2;3)'.AST), ((1, 2), 3), 'EVAL of a converted multi-section list';
is-deeply EVAL('(1;2)'.AST), (1, 2), 'EVAL of a converted two-item list';
is-deeply (1,2;3), ((1, 2), 3), 'the list itself is unchanged';

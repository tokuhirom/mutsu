use v6;
use experimental :rakuast;
use Test;

# A comma list of declarations is one `ApplyListInfix` whose operands are the
# declarations, however many there are (measured on rakudo 2026.09). The
# parser keeps the list as written and lowers it once, so a third declaration
# is a sibling operand, not a nested list.

plan 10;

my $three = Q[my $a = 1, my $b = 2, my $c = 3;].AST.statements[0].expression;
isa-ok $three, RakuAST::ApplyListInfix, 'three declarations are one comma application';
is $three.operands.elems, 3, 'with three operands';
ok $three.operands.all ~~ RakuAST::VarDeclaration::Simple, 'each operand is a declaration';

my $two = Q[my $x = 1, my $y;].AST.statements[0].expression;
is $two.operands.elems, 2, 'two declarations, the second without an initializer';

my $loop = Q[loop (my $i = 0, my $j = 5; $i < 2; $i++) { say 1 }].AST.statements[0];
isa-ok $loop, RakuAST::Statement::Loop, 'a C-style loop keeps its header';

is EVAL(Q[my $a = 1, my $b = 2, my $c = 3; "$a$b$c"].AST), '123',
    'the declarations all run, in order';

my $log = '';
is EVAL(Q[my $r = ''; loop (my $i = 0, my $j = 5; $i < 2; $i++) { $r ~= "$i$j," }; $r].AST),
    '05,15,', 'loop-header declarations run once and see each other';

# An attribute with a postfix modifier keeps the modifier on the declaration.
my $class = Q[class A { has $.x if True }].AST.statements[0].expression;
my $attribute = $class.body.body.statement-list.statements[0];
isa-ok $attribute.condition-modifier, RakuAST::StatementModifier::If,
    'the attribute keeps its postfix `if`';

# A phaser over a bare statement is the prefix's child.
my $phaser = Q[LEAVE say 1].AST.statements[0].expression;
isa-ok $phaser, RakuAST::StatementPrefix::Phaser::Leave, 'a bare-statement phaser is a statement prefix';

is EVAL(Q[my $r = 0; { LEAVE $r = 5 }; $r].AST), 5, 'its statement runs at scope exit';

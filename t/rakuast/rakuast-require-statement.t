use v6;
use lib 'roast/packages/S11-modules/lib';
use Test;
use experimental :rakuast;

# A literal require is a Statement::Require even inside an expression. Its
# lowered target must stay a Package value so lexical stubs are hoisted.
plan 6;

my $direct = 'require InnerModule;'.AST.statements[0];
isa-ok $direct, RakuAST::Statement::Require, 'bare require is its own statement';
is $direct.module-name.canonicalize, 'InnerModule', 'the target is a Name';
is EVAL('require InnerModule;'.AST).^name, 'InnerModule',
    'a round-tripped require loads the module';

my $nested = 'my $x = (require InnerModule);'.AST.statements[0].expression;
my $inside = $nested.initializer.expression.semilist.statements[0];
isa-ok $inside, RakuAST::Statement::Require,
    'require inside parentheses has no expression wrapper';
is EVAL('my $x = (require InnerModule); $x.^name'.AST), 'InnerModule',
    'an expression-position require returns the module';

is EVAL('try require MissingRakuAstStub7564; ::("MissingRakuAstStub7564").^name'.AST),
    'MissingRakuAstStub7564', 'a failed require leaves its lexical stub';

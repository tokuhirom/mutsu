use v6;
use Test;
use experimental :rakuast;

# `require Module <&sym ...>` keeps its import list as the statement's
# `argument` (a words QuotedString), and an EVAL string run from inside a
# module sees the required package as a global name.
plan 6;

my $ast = 'require Test <&plan &is>;'.AST.statements[0];
isa-ok $ast, RakuAST::Statement::Require, 'a require with an import list';
isa-ok $ast.argument, RakuAST::QuotedString, 'the import list is a QuotedString';
is $ast.module-name.canonicalize, 'Test', 'the module name is kept';

my $r = EVAL('require Test <&plan &is>;'.AST);
is $r.^name, 'Test', 'the round-tripped require returns the package';
is EVAL('my $x = (require Test <&plan>); $x.gist'.AST), '(Test)',
    'an expression-position require with imports returns the package';

require Test <&eval-lives-ok>;
eval-lives-ok q|my $x; EVAL '$x = Test'; die unless $x.gist eq "(Test)";|,
    'a nested EVAL inside a module sees the required package';

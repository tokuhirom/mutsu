use Test;
use experimental :rakuast;

plan 8;

# `::?CLASS` / `::?PACKAGE` as a term are `Var::Lexical::Constant` (#14845).
my $ast = 'class C { method m { ::?CLASS.^name } }'.AST;
my @c = $ast.find-nodes(RakuAST::Var::Lexical::Constant);
is @c.elems, 1, 'one constant node';
is @c[0].name, '::?CLASS', 'its name is the pseudo-package as written';
is @c[0].raku, 'RakuAST::Var::Lexical::Constant.new("::?CLASS")', '.raku';
is-deeply $ast.DEPARSE.lines.map(*.trim).join(' '), 'class C { method m { ::?CLASS.^name } }', 'DEPARSE keeps ::?CLASS';
is 'package P { say ::?PACKAGE.^name }'.AST.DEPARSE.lines.map(*.trim).join(' '), 'package P { say ::?PACKAGE.^name }', '::?PACKAGE';
is 'role R { method m { ::?ROLE.^name } }'.AST.DEPARSE.lines.map(*.trim).join(' '), 'role R { method m { ::?ROLE.^name } }', '::?ROLE';
is RakuAST::Var::Lexical::Constant.new("::?CLASS").name, '::?CLASS', 'constructible with its name';
is EVAL('class D { method m { ::?CLASS.^name } }; D.new.m'.AST), 'D', 'EVAL of the AST resolves it at run time';

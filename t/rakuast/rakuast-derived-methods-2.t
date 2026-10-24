use Test;

# More methods rakudo computes from a node's attributes, measured on rakudo
# 2026.09 (the second wave rakudo's own RakuAST::Deparse needed). This file
# passes under both mutsu and raku.

plan 24;

sub expr(Str $src) { $src.AST.statements[0].expression }

is expr(Q[sub f($a?) {}]).signature.parameters[0].is-declared-optional, 1, 'declared optional';
is expr(Q[sub f($a) {}]).signature.parameters[0].is-declared-optional, 0, 'not declared optional';
is expr(Q[sub f($a!) {}]).signature.parameters[0].is-declared-required, 1, 'declared required';
is expr(Q[sub f(:$a) {}]).signature.parameters[0].is-declared-optional, 0, 'a named parameter';
is expr(Q[sub f($a) {}]).signature.parameters[0].outer-type, 0, 'no outer type';

nok expr(Q[class C1 {}]).parameterization.defined, 'a class has no parameterization';
is expr(Q[role R1[$x] {}]).parameterization.parameters.elems, 1, 'a parameterized role';
is expr(Q[role R2 {}]).parameterization.parameters.elems, 0, 'a plain role';
is expr(Q[class P3 {}]).parsed-declarator, 'class', 'parsed-declarator';

is expr(Q[Less]).name.canonicalize, 'Less', 'Term::Enum.name is a Name';
nok RakuAST::Name.from-identifier("a").is-indirect-lookup, 'a plain name is no indirect lookup';

is expr(Q[a => 1]).named-arg-name, 'a', 'FatArrow.named-arg-name';
is expr(Q[:a(1)]).named-arg-name, 'a', 'ColonPair.named-arg-name';
is expr(Q[:a(1)]).named-arg-value.^name, 'RakuAST::Circumfix::Parentheses', 'ColonPair.named-arg-value';
is expr(Q[FIRST 1]).original-blorst.^name, 'RakuAST::Statement::Expression', 'Phaser.original-blorst';

is Q[if 1 { }].AST.statements[0].IMPL-QAST-TYPE, 'if', 'If.IMPL-QAST-TYPE';
is Q[with 1 { }].AST.statements[0].IMPL-QAST-TYPE, 'with', 'With.IMPL-QAST-TYPE';
is expr(Q[sub f() is export {}]).traits[0].IMPL-TRAIT-NAME, 'is', 'Trait::Is.IMPL-TRAIT-NAME';

is expr(Q[1.^name]).postfix.args.^name, 'RakuAST::ArgList', 'MetaMethod.args';
is expr(Q[...]).name, '...', 'Stub::Fail.name';
is expr(Q[!!!]).name, '!!!', 'Stub::Die.name';
is expr(Q[{ @_ }]).body.statement-list.statements[0].expression.lexical-name, '@_', 'slurpy placeholder lexical-name';
is expr(qq[q:to/END/;\nabc\nEND\n]).has-variables, False, 'a plain heredoc has no variables';
is expr(qq[q:to/END/;\nabc\nEND\n]).processors.elems, 0, 'Heredoc.processors';

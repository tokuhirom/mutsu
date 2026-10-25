use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# `.= meth` written with no target is the topic's `.=`: `.AST` shows a
# `Term::TopicCall` whose method carries the `.=` dispatch, unlike
# `$_ .= meth` (an `ApplyDottyInfix`), as on rakudo 2026.09. The tree lowers to
# what the parsed program does (ADR-10723 Stage 2, #14948). This file passes
# under both mutsu and raku.

plan 13;

sub last-statement(Str $src) { $src.AST.statements[*-1].expression }

my $stmt = last-statement(Q[.=uc]);
is $stmt.^name, 'RakuAST::Term::TopicCall', 'a bare .= statement is a topic call';
is $stmt.call.dispatch, '.=', 'whose method carries the .= dispatch';
is $stmt.call.name.canonicalize, 'uc', 'over the method name';
is last-statement(Q[$_ .= uc]).^name, 'RakuAST::ApplyDottyInfix', '$_ .= stays a dotty infix';
is last-statement(Q[my $x; $x andthen .=new: 43]).operands[1].^name, 'RakuAST::Term::TopicCall',
    'a bare .= in an andthen chain';
is last-statement(Q[say(.=uc)]).args.args[0].call.dispatch, '.=', 'a bare .= in an argument';

# The trees run like the parsed program.
is EVAL(Q[$_ = "a"; .=uc; $_].AST), 'A', 'as a statement';
is EVAL(Q[my @a = <a b>; given @a { .=reverse }; @a.join(",")].AST), 'b,a',
    'writes through a whole-container topic';
is EVAL(Q[$_ = "a"; my $r = (.=uc); $r].AST), 'A', 'in parentheses';
is EVAL(Q[my $x = 1; $x andthen .=new: 43; $x].AST), 43, 'in an andthen chain';
is EVAL(Q[$_ = "a"; my $r = 1 ?? .=uc !! 2; $_].AST), 'A', 'unparenthesized in a ternary branch';
is EVAL(Q[$_ = "ab"; .=substr(1); $_].AST), 'b', 'with arguments';

# Rebuilding a tree from its `.raku` gives the same tree.
my $ast = Q[.=uc].AST;
is EVAL($ast.raku).raku, $ast.raku, 'round trip';

use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a postfix operator on the
# topic (`.++`) is placed after its dot; a loop statement in parentheses is
# placed; a call on a parenthesized curry (`(* + *)(3, 4)`) places the curry
# and the call.

plan 4;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[$_ = 4; .++; .--]),
    'ApplyInfix@0-6 Assignment@3-4 ArgList@0-6 Var::Lexical@0-2 IntLiteral@5-6 Term::TopicCall@8-11 Postfix@9-11 Term::TopicCall@13-16 Postfix@14-16',
    'postfix operators on the topic';
is placed(Q[my $n = 0; my $r = (repeat { $n++ } while $n < 3)]),
    'VarDeclaration::Simple@0-9 Initializer::Assign@6-9 IntLiteral@8-9 VarDeclaration::Simple@11-49 Initializer::Assign@17-49 Circumfix::Parentheses@19-49 SemiList@20-48 ApplyInfix@42-48 Infix@45-46 ArgList@42-48 Var::Lexical@42-44 IntLiteral@47-48 Block@27-35 Blockoid@27-35 ApplyPostfix@29-33 Var::Lexical@29-31 Postfix@31-33',
    'a repeat loop in parentheses';
is placed(Q[my $q = (loop { last })]),
    'VarDeclaration::Simple@0-23 Initializer::Assign@6-23 Circumfix::Parentheses@8-23 SemiList@9-22 Block@14-22 Blockoid@14-22 Call::Name::WithoutParentheses@16-20 Name@16-20 ArgList@20-20',
    'a bare loop in parentheses';
is placed(Q[say (* + *)(3, 4)]),
    'Call::Name::WithoutParentheses@0-17 Name@0-3 ArgList@4-17 ApplyPostfix@4-17 ApplyInfix@5-10 Infix@7-8 ArgList@5-10 Call::Term@11-17 ArgList@12-16 IntLiteral@12-13 IntLiteral@15-16',
    'a call on a parenthesized curry';

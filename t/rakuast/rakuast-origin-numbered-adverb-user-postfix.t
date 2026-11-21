use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a numbered regex adverb
# (`:2nd`) is placed over its text; `*!` with a user postfix curries into a
# placed WhateverCode; an indirect package name with a tail (`$::($u)::y7`)
# runs from after its sigil to its end; the statements of a one-line
# anonymous `sub` body are placed from the body's text.

plan 5;
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
is placed(Q[my $s = "abab"; $s ~~ s:2nd/b/Y/]),
    'VarDeclaration::Simple@0-14 Initializer::Assign@6-14 QuotedString@8-14 StrLiteral@9-13 ApplyInfix@16-32 Infix@19-21 ArgList@16-32 Var::Lexical@16-18 Substitution@22-32 ColonPair::Number@23-27 Regex::Literal@28-29 QuotedString@30-31 StrLiteral@30-31',
    'a numbered substitution adverb';
is placed(Q[my $s = "aXa"; say $s ~~ m:2nd/a/]),
    'VarDeclaration::Simple@0-13 Initializer::Assign@6-13 QuotedString@8-13 StrLiteral@9-12 Call::Name::WithoutParentheses@15-33 Name@15-18 ArgList@19-33 ApplyInfix@19-33 Infix@22-24 ArgList@19-33 Var::Lexical@19-21 QuotedRegex@25-33 ColonPair::Number@26-30 Regex::Literal@31-32',
    'a numbered match adverb';
is placed(Q[sub postfix:<!>($n) { [*] 1..$n }; say (*!)(4)]),
    'Sub@0-33 Name@4-15 QuotedString@12-15 StrLiteral@13-14 Signature@16-18 Parameter@16-18 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Blockoid@20-33 Term::Reduce@22-31 Infix@23-24 ArgList@26-31 ApplyInfix@26-31 Infix@27-29 ArgList@26-31 IntLiteral@26-27 Var::Lexical@29-31 Call::Name::WithoutParentheses@35-46 Name@35-38 ArgList@39-46 ApplyPostfix@39-46 ApplyPostfix@40-42 Postfix@41-42 Call::Term@43-46 ArgList@44-45 IntLiteral@44-45',
    'a user postfix applied to a Whatever';
is placed(Q[my $u = "GLOBAL"; our $y7 = 9; say $::($u)::y7]),
    'VarDeclaration::Simple@0-16 Initializer::Assign@6-16 QuotedString@8-16 StrLiteral@9-15 VarDeclaration::Simple@18-29 Initializer::Assign@26-29 IntLiteral@28-29 Call::Name::WithoutParentheses@31-46 Name@31-34 ArgList@35-46 Var::Package@35-46 Name@36-46 Var::Lexical@39-41',
    'an indirect package name with a tail';
is placed(Q[my &f = sub ($a, $b) { my $c = $a; $c + $b }]),
    'VarDeclaration::Simple@0-44 Initializer::Assign@6-44 Sub@8-44 Signature@13-19 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Parameter@17-19 ParameterTarget::Var@17-19 VarDeclaration::Simple@17-19 Blockoid@21-44 VarDeclaration::Simple@23-33 Initializer::Assign@29-33 Var::Lexical@31-33 ApplyInfix@35-42 Infix@38-39 ArgList@35-42 Var::Lexical@35-37 Var::Lexical@40-42',
    'the statements of a one-line anonymous sub body';

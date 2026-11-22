use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for a fifth set of
# constructs the roast corpus showed unplaced. `[!after]` is also
# `MetaInfix::Negate` over `Infix("after")`, as in Rakudo.

plan 14;
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
is placed(Q[say «a "b c" d»]),
    'Call::Name::WithoutParentheses@0-15 Name@0-3 ArgList@4-15 QuotedString@4-15 StrLiteral@5-7 QuoteWordsAtom@7-12 QuotedString@7-12 StrLiteral@8-11 StrLiteral@12-14',
    'a quoted word in an interpolating word quote';
is placed(Q[say «"x" -e "y"»]),
    'Call::Name::WithoutParentheses@0-16 Name@0-3 ArgList@4-16 QuotedString@4-16 QuoteWordsAtom@5-8 QuotedString@5-8 StrLiteral@6-7 StrLiteral@8-12 QuoteWordsAtom@12-15 QuotedString@12-15 StrLiteral@13-14',
    'quoted words around a bare word';
is placed(Q[class A { method foo(A:D:) { 1 } }]),
    'Class@0-34 Name@6-7 Block@8-34 Blockoid@8-34 Method@10-32 Name@17-20 Signature@21-25 Parameter@21-24 Type::Definedness@21-24 Type::Simple@21-24 Name@21-22 Blockoid@27-32 IntLiteral@29-30',
    'an anonymous typed invocant';
is placed(Q[class B { method !foo(B:D: $x) { 1 } }]),
    'Class@0-38 Name@6-7 Block@8-38 Blockoid@8-38 Method@10-36 Name@18-21 Signature@22-29 Parameter@22-25 Type::Definedness@22-25 Type::Simple@22-25 Name@22-23 Parameter@27-29 ParameterTarget::Var@27-29 VarDeclaration::Simple@27-29 Blockoid@31-36 IntLiteral@33-34',
    'a typed invocant before a parameter';
is placed(Q[my $x; say "a $x.ord.fmt('%d')"]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-31 Name@7-10 ArgList@11-31 QuotedString@11-31 StrLiteral@12-14 ApplyPostfix@14-30 ApplyPostfix@14-20 Var::Lexical@14-16 Call::Method@17-20 Name@17-20 ArgList@20-20 Call::Method@21-30 Name@21-24 ArgList@25-29 QuotedString@25-29 StrLiteral@26-28',
    'a chain of method calls in a string';
is placed(Q[my $x; say "\$x is $x.ord.chr()"]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-32 Name@7-10 ArgList@11-32 QuotedString@11-32 StrLiteral@12-19 ApplyPostfix@19-31 ApplyPostfix@19-25 Var::Lexical@19-21 Call::Method@22-25 Name@22-25 ArgList@25-25 Call::Method@26-31 Name@26-29 ArgList@30-30',
    'a chain after an escaped sigil';
is placed(Q[module D { our token doo { doo } }; say "doo" ~~ / <&D::doo> /]),
    'Module@0-34 Name@7-8 Block@9-34 Blockoid@9-34 TokenDeclaration@11-32 Name@21-24 Regex::Sequence@27-31 Regex::WithWhitespace@27-31 Regex::Literal@27-30 Call::Name::WithoutParentheses@36-62 Name@36-39 ArgList@40-62 ApplyInfix@40-62 Infix@46-48 ArgList@40-62 QuotedString@40-45 StrLiteral@41-44 QuotedRegex@49-62 Regex::WithWhitespace@51-61 Regex::Assertion::Callable@51-60 Var::Package@52-59 Name@53-59',
    'a package-qualified callable assertion';
is placed(Q「say [!after] 1, 2」),
    'Call::Name::WithoutParentheses@0-17 Name@0-3 ArgList@4-17 Term::Reduce@4-17 MetaInfix::Negate@5-11 Infix@6-11 ArgList@13-17 IntLiteral@13-14 IntLiteral@16-17',
    'a negated reduction';
is placed(Q「say [\R-] 1, 2」),
    'Call::Name::WithoutParentheses@0-14 Name@0-3 ArgList@4-14 Term::Reduce@4-14 MetaInfix::Reverse@6-8 Infix@7-8 ArgList@10-14 IntLiteral@10-11 IntLiteral@13-14',
    'a reversed triangular reduction';
is placed(Q[my $x; start until $x { 1 }]),
    'VarDeclaration::Simple@0-5 Var::Lexical@19-21 Block@22-27 Blockoid@22-27 IntLiteral@24-25',
    'a loop under start';
is placed(Q[my $x = do loop (my $i = 0; $i < 2; $i++) { 1 }]),
    'VarDeclaration::Simple@0-47 Initializer::Assign@6-47 VarDeclaration::Simple@17-26 Initializer::Assign@23-26 IntLiteral@25-26 ApplyInfix@28-34 Infix@31-32 ArgList@28-34 Var::Lexical@28-30 IntLiteral@33-34 ApplyPostfix@36-40 Var::Lexical@36-38 Postfix@38-40 Block@42-47 Blockoid@42-47 IntLiteral@44-45',
    'a C-style loop under do';
is placed(Q[my $s; say &CALLER::($s)::not]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-29 Name@7-10 ArgList@11-29 Var::Package@11-29 Name@12-29 Var::Lexical@21-23',
    'an indirect name between simple parts';
is Q[say [!after] 1, 2].AST.statements[0].expression.args.args[0].infix.^name,
    'RakuAST::MetaInfix::Negate', 'a negated reduction wraps its infix';
is Q[say [!=] 1, 2].AST.statements[0].expression.args.args[0].infix.^name,
    'RakuAST::Infix', '[!=] is an infix of its own';

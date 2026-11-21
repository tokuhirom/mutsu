use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a word prefix (`so`,
# `not`) is placed over the word and its operand, and a parenthesized call
# followed by a dotted subscript (`f(1).{"a"}`) places the call.

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
is placed(Q[my @a = 1, so 2 > 1]),
    'VarDeclaration::Simple@0-19 Initializer::Assign@6-19 ApplyListInfix@8-19 Infix@9-10 IntLiteral@8-9 ApplyPrefix@11-19 Prefix@11-13 ApplyInfix@14-19 Infix@16-17 ArgList@14-19 IntLiteral@14-15 IntLiteral@18-19',
    'a word prefix in a list';
is placed(Q[say not  1]),
    'Call::Name::WithoutParentheses@0-10 Name@0-3 ArgList@4-10 ApplyPrefix@4-10 Prefix@4-7 IntLiteral@9-10',
    'a word prefix after blanks';
is placed(Q[sub f($x) { 1 }; say f(1).{"a"}]),
    'Sub@0-15 Name@4-5 Signature@6-8 Parameter@6-8 ParameterTarget::Var@6-8 VarDeclaration::Simple@6-8 Blockoid@10-15 IntLiteral@12-13 Call::Name::WithoutParentheses@17-31 Name@17-20 ArgList@21-31 ApplyPostfix@21-31 Call::Name@21-25 Name@21-22 ArgList@23-24 IntLiteral@23-24 Postcircumfix::HashIndex@26-31 SemiList@27-30 QuotedString@27-30 StrLiteral@28-29',
    'a call with a dotted hash subscript';
is placed(Q[sub f($x) { 1 }; say f(1).<a>]),
    'Sub@0-15 Name@4-5 Signature@6-8 Parameter@6-8 ParameterTarget::Var@6-8 VarDeclaration::Simple@6-8 Blockoid@10-15 IntLiteral@12-13 Call::Name::WithoutParentheses@17-29 Name@17-20 ArgList@21-29 ApplyPostfix@21-29 Call::Name@21-25 Name@21-22 ArgList@23-24 IntLiteral@23-24 Postcircumfix::LiteralHashIndex@26-29 QuotedString@27-28 StrLiteral@27-28',
    'a call with a dotted word subscript';

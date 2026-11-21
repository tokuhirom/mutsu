use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the argument list of a
# `use` statement is placed; an anonymous `my regex` declaration runs from
# its `my`; a callable regex assertion with arguments (`<&$p("b")>`,
# `<&q("b")>`) places its callee and its `ArgList`.

plan 3;
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
is placed(Q[use Test :DEFAULT, :ALL;]),
    'Name@4-8 ApplyListInfix@9-23 Infix@17-18 ColonPair::True@9-17 ColonPair::True@19-23',
    'the arguments of a use statement';
is placed(Q[my $p = my regex ($x) { $x }; say "b" ~~ / <&$p("b")> /]),
    'VarDeclaration::Simple@0-28 Initializer::Assign@6-28 RegexDeclaration@8-28 Signature@18-20 Parameter@18-20 ParameterTarget::Var@18-20 VarDeclaration::Simple@18-20 Regex::WithWhitespace@24-27 Regex::Interpolation@24-26 Var::Lexical@24-26 Call::Name::WithoutParentheses@30-55 Name@30-33 ArgList@34-55 ApplyInfix@34-55 Infix@38-40 ArgList@34-55 QuotedString@34-37 StrLiteral@35-36 QuotedRegex@41-55 Regex::WithWhitespace@43-54 Regex::Assertion::Callable@43-53 Contextualizer::Item@44-47 Var::Lexical@45-47 ArgList@48-51 QuotedString@48-51 StrLiteral@49-50',
    'an anonymous my regex and a callable assertion with arguments';
is placed(Q[my &q; say "b" ~~ / <&q("b")> /]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-31 Name@7-10 ArgList@11-31 ApplyInfix@11-31 Infix@15-17 ArgList@11-31 QuotedString@11-14 StrLiteral@12-13 QuotedRegex@18-31 Regex::WithWhitespace@20-30 Regex::Assertion::Callable@20-29 Var::Lexical@21-23 ArgList@24-27 QuotedString@24-27 StrLiteral@25-26',
    'a code variable assertion with arguments';

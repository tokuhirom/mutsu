use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a declaration's `of TYPE`
# trait is placed, and its type is the declaration's `.type`, visited first;
# an imaginary literal places its number and `i`; a subscript on the topic
# (`.<a>`, `.[0]`) is placed after its dot.

plan 6;
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
is placed(Q[my @arr of Int = 1, 2]),
    'VarDeclaration::Simple@0-21 Type::Simple@11-14 Name@11-14 Initializer::Assign@15-21 ApplyListInfix@17-21 Infix@18-19 IntLiteral@17-18 IntLiteral@20-21 Trait::Of@8-14 Type::Simple@11-14 Name@11-14',
    'an of trait, visited as the declaration type';
is placed(Q[my $x of Int is default(3)]),
    'VarDeclaration::Simple@0-26 Type::Simple@9-12 Name@9-12 Trait::Of@6-12 Type::Simple@9-12 Name@9-12 Trait::Is@13-26 Name@16-23 Circumfix::Parentheses@23-26 SemiList@24-25 IntLiteral@24-25',
    'an of trait before an is trait';
is placed(Q[say 0.0i, 3e2i, 2i]),
    'Call::Name::WithoutParentheses@0-18 Name@0-3 ArgList@4-18 ApplyPostfix@4-8 RatLiteral@4-7 Postfix@7-8 ApplyPostfix@10-14 NumLiteral@10-13 Postfix@13-14 ApplyPostfix@16-18 IntLiteral@16-17 Postfix@17-18',
    'imaginary literals';
is placed(Q[my %h; given %h { say .<a> }]),
    'VarDeclaration::Simple@0-5 Var::Lexical@13-15 Block@16-28 Blockoid@16-28 Call::Name::WithoutParentheses@18-26 Name@18-21 ArgList@22-26 Term::TopicCall@22-26 Postcircumfix::LiteralHashIndex@23-26 QuotedString@24-25 StrLiteral@24-25',
    'a topic word subscript';
is placed(Q[my @a; given @a { say .[0] }]),
    'VarDeclaration::Simple@0-5 Var::Lexical@13-15 Block@16-28 Blockoid@16-28 Call::Name::WithoutParentheses@18-26 Name@18-21 ArgList@22-26 Term::TopicCall@22-26 Postcircumfix::ArrayIndex@23-26 SemiList@24-25 IntLiteral@24-25',
    'a topic array subscript';
is Q[my @arr of Int].AST.statements[0].expression.type.raku,
    'RakuAST::Type::Simple.from-identifier("Int")',
    'the of type is the declaration type';

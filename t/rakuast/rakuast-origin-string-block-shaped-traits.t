use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: statements inside a block
# interpolated in a string that the parser left unplaced (`"{:a(7)}"`) are
# read again from the text between the braces, and the traits of a shaped
# variable (`my %h{Str} of Int`) are read after its shape.

plan 5;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[say "{:a(7)}"]),
    'StatementList@0-13 Statement::Expression@0-13 Call::Name::WithoutParentheses@0-13 Name@0-3 ArgList@4-13 QuotedString@4-13 Block@5-12 Blockoid@5-12 StatementList@6-11 Statement::Expression@6-11 ColonPair::Value@6-11 Circumfix::Parentheses@8-11 SemiList@9-10 Statement::Expression@9-10 IntLiteral@9-10',
    'a colonpair with a value in an interpolated block';
is placed(Q[say "x{:a}y{1; 2}"]),
    'StatementList@0-18 Statement::Expression@0-18 Call::Name::WithoutParentheses@0-18 Name@0-3 ArgList@4-18 QuotedString@4-18 StrLiteral@5-6 Block@6-10 Blockoid@6-10 StatementList@7-9 Statement::Expression@7-9 ColonPair::True@7-9 StrLiteral@10-11 Block@11-17 Blockoid@11-17 StatementList@12-16 Statement::Expression@12-13 IntLiteral@12-13 Statement::Expression@15-16 IntLiteral@15-16',
    'a colonpair and a statement sequence in interpolated blocks';
is placed(Q[my %h{Str} of Int = a => 1]),
    'StatementList@0-26 Statement::Expression@0-26 VarDeclaration::Simple@0-26 Type::Simple@14-17 Name@14-17 Initializer::Assign@18-26 FatArrow@20-26 IntLiteral@25-26 SemiList@6-9 Statement::Expression@6-9 Type::Simple@6-9 Name@6-9 Trait::Of@11-17 Type::Simple@14-17 Name@14-17',
    'an of trait after a shaped hash';
is placed(Q[my @a[3] of Int]),
    'StatementList@0-15 Statement::Expression@0-15 VarDeclaration::Simple@0-15 Type::Simple@12-15 Name@12-15 SemiList@6-7 Statement::Expression@6-7 IntLiteral@6-7 Trait::Of@9-15 Type::Simple@12-15 Name@12-15',
    'an of trait after a shaped array';
is placed(Q[my %h{Any} is default(1)]),
    'StatementList@0-24 Statement::Expression@0-24 VarDeclaration::Simple@0-24 SemiList@6-9 Statement::Expression@6-9 Type::Simple@6-9 Name@6-9 Trait::Is@11-24 Name@14-21 Circumfix::Parentheses@21-24 SemiList@22-23 Statement::Expression@22-23 IntLiteral@22-23',
    'an is trait after a shaped hash';

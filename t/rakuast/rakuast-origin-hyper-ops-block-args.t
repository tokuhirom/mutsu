use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a hyper infix application
# places its `MetaInfix::Hyper` and the operator inside (`>>+<<`,
# `>>[&plus]<<`, `»~»`); a listop call starting with a block argument places
# the block; a quoted regex's adverb with a value (`m:p($pos)`) places it.

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
is placed(Q[my @a; my @b; say (@a >>+<< @b)]),
    'VarDeclaration::Simple@0-5 VarDeclaration::Simple@7-12 Call::Name::WithoutParentheses@14-31 Name@14-17 ArgList@18-31 Circumfix::Parentheses@18-31 SemiList@19-30 ApplyInfix@19-30 MetaInfix::Hyper@22-27 Infix@24-25 ArgList@19-30 Var::Lexical@19-21 Var::Lexical@28-30',
    'a hyper operator';
is placed(Q[sub plus($x, $y) { 1 }; my @a; say (@a >>[&plus]<< @a)]),
    'Sub@0-22 Name@4-8 Signature@9-15 Parameter@9-11 ParameterTarget::Var@9-11 VarDeclaration::Simple@9-11 Parameter@13-15 ParameterTarget::Var@13-15 VarDeclaration::Simple@13-15 Blockoid@17-22 IntLiteral@19-20 VarDeclaration::Simple@24-29 Call::Name::WithoutParentheses@31-54 Name@31-34 ArgList@35-54 Circumfix::Parentheses@35-54 SemiList@36-53 ApplyInfix@36-53 MetaInfix::Hyper@39-50 FunctionInfix@41-48 Var::Lexical@42-47 ArgList@36-53 Var::Lexical@36-38 Var::Lexical@51-53',
    'a hyper function operator';
is placed(Q[my @a; say @a »~» 1]),
    'VarDeclaration::Simple@0-5 Call::Name::WithoutParentheses@7-19 Name@7-10 ArgList@11-19 ApplyInfix@11-19 MetaInfix::Hyper@14-17 Infix@15-16 ArgList@11-19 Var::Lexical@11-13 IntLiteral@18-19',
    'a Unicode hyper operator';
is placed(Q[sub refused(&b, $w) { }; refused { 1 }, "y"]),
    'Sub@0-23 Name@4-11 Signature@12-18 Parameter@12-14 ParameterTarget::Var@12-14 VarDeclaration::Simple@12-14 Parameter@16-18 ParameterTarget::Var@16-18 VarDeclaration::Simple@16-18 Blockoid@20-23 Call::Name::WithoutParentheses@25-43 Name@25-32 ArgList@33-43 Block@33-38 Blockoid@33-38 IntLiteral@35-36 QuotedString@40-43 StrLiteral@41-42',
    'a block argument to a listop call';
is placed(Q[my $pos = 0; say "abc" ~~ m:p($pos)/ abc /]),
    'VarDeclaration::Simple@0-11 Initializer::Assign@8-11 IntLiteral@10-11 Call::Name::WithoutParentheses@13-42 Name@13-16 ArgList@17-42 ApplyInfix@17-42 Infix@23-25 ArgList@17-42 QuotedString@17-22 StrLiteral@18-21 QuotedRegex@26-42 ColonPair::Value@27-35 Circumfix::Parentheses@29-35 SemiList@30-34 Var::Lexical@30-34 Regex::Sequence@37-41 Regex::WithWhitespace@37-41 Regex::Literal@37-40',
    'a regex adverb with a variable value';
is placed(Q[say "ab" ~~ m:c(0)/ a /]),
    'Call::Name::WithoutParentheses@0-23 Name@0-3 ArgList@4-23 ApplyInfix@4-23 Infix@9-11 ArgList@4-23 QuotedString@4-8 StrLiteral@5-7 QuotedRegex@12-23 ColonPair::Value@13-18 Circumfix::Parentheses@15-18 SemiList@16-17 IntLiteral@16-17 Regex::WithWhitespace@20-22 Regex::Literal@20-21',
    'a regex adverb with a literal value';

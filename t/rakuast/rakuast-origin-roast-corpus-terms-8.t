use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for an eighth set of
# constructs the roast corpus showed unplaced or shaped differently: a
# parenthesized `*` and a bare `*` condition are `Term::Whatever`, and an
# attribute's `of TYPE` is a `Trait::Of`, as in Rakudo.

plan 7;
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
is placed(Q|grammar G2 { proto token a { * } }|),
    'Grammar@0-34 Name@8-10 Block@11-34 Blockoid@11-34 TokenDeclaration@13-32 Name@25-26 OnlyStar@31-31',
    'the OnlyStar of a proto token';
is placed(Q|class R2 { has @.a of int }|),
    'Class@0-27 Name@6-8 Block@9-27 Blockoid@9-27 VarDeclaration::Simple@11-25 Type::Simple@22-25 Name@22-25 Trait::Of@19-25 Type::Simple@22-25 Name@22-25',
    'an attribute of-type as a Trait::Of';
is placed(Q|given 1 { when * { } }|),
    'IntLiteral@6-7 Block@8-22 Blockoid@8-22 Term::Whatever@15-16 Block@17-20 Blockoid@17-20',
    'a bare whatever when condition';
is placed(Q|say (((*)))|),
    'Call::Name::WithoutParentheses@0-11 Name@0-3 ArgList@4-11 Circumfix::Parentheses@4-11 SemiList@5-10 Circumfix::Parentheses@5-10 SemiList@6-9 Circumfix::Parentheses@6-9 SemiList@7-8 Term::Whatever@7-8',
    'a parenthesized whatever is a value';
is placed(Q|grammar G4 { token a:sym<foo> { x } }|),
    'Grammar@0-37 Name@8-10 Block@11-37 Blockoid@11-37 TokenDeclaration@13-35 Name@19-29 ColonPair::Value@20-29 QuotedString@24-29 StrLiteral@25-28 Regex::WithWhitespace@32-34 Regex::Literal@32-33',
    'a sym token name';
is placed(Q|grammar G5 { token a:sym«<foo>» { x } }|),
    'Grammar@0-39 Name@8-10 Block@11-39 Blockoid@11-39 TokenDeclaration@13-37 Name@19-31 ColonPair::Value@20-31 QuotedString@24-31 StrLiteral@25-30 Regex::WithWhitespace@34-36 Regex::Literal@34-35',
    'a French-quoted sym holding angle brackets';
is placed(Q|grammar G6 { token b:sym<<bar>> { x } }|),
    'Grammar@0-39 Name@8-10 Block@11-39 Blockoid@11-39 TokenDeclaration@13-37 Name@19-31 ColonPair::Value@20-31 QuotedString@24-31 StrLiteral@26-29 Regex::WithWhitespace@34-36 Regex::Literal@34-35',
    'a double-angle sym';

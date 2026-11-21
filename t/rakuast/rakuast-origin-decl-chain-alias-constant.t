use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: a chain of calls on a
# declaration (`my $n.WHAT.^name`), a regex alias to a non-capturing call
# with arguments (`<alias=.word("a")>`), and the traits of a constant, which
# Rakudo visits after its initializer.

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
is placed(Q[say my $n.WHAT.^name]),
    'Call::Name::WithoutParentheses@0-20 Name@0-3 ArgList@4-20 ApplyPostfix@4-20 ApplyPostfix@4-14 VarDeclaration::Simple@4-9 Call::Method@10-14 Name@10-14 ArgList@14-14 Call::MetaMethod@16-20 ArgList@20-20',
    'a chained call on a declaration in an argument';
is placed(Q[my $n.WHAT.Str.say]),
    'ApplyPostfix@0-18 ApplyPostfix@0-14 ApplyPostfix@0-10 VarDeclaration::Simple@0-5 Call::Method@6-10 Name@6-10 ArgList@10-10 Call::Method@11-14 Name@11-14 ArgList@14-14 Call::Method@15-18 Name@15-18 ArgList@18-18',
    'a chained call on a declaration';
is placed(Q[grammar G1 { token TOP { <alias=.word("a")> }; token word($x) { $x } }]),
    'Grammar@0-70 Name@8-10 Block@11-70 Blockoid@11-70 TokenDeclaration@13-45 Name@19-22 Regex::WithWhitespace@25-44 Regex::Assertion::Alias@25-43 Regex::Assertion::Named::Args@33-42 Name@33-37 ArgList@38-41 QuotedString@38-41 StrLiteral@39-40 TokenDeclaration@47-68 Name@53-57 Signature@58-60 Parameter@58-60 ParameterTarget::Var@58-60 VarDeclaration::Simple@58-60 Regex::WithWhitespace@64-67 Regex::Interpolation@64-66 Var::Lexical@64-66',
    'an alias to a non-capturing call with arguments';
is placed(Q[my constant \C1 is export(:colors) = 1]),
    'VarDeclaration::Constant@0-38 Initializer::Assign@35-38 IntLiteral@37-38 Trait::Is@16-34 Name@19-25 Circumfix::Parentheses@25-34 SemiList@26-33 ColonPair::True@26-33',
    'a constant with a trait argument, sigilless';
is placed(Q[constant D1 is export = 1]),
    'VarDeclaration::Constant@0-25 Initializer::Assign@22-25 IntLiteral@24-25 Trait::Is@12-21 Name@15-21',
    'a constant with a trait';

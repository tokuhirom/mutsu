use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for a seventh set of
# constructs the roast corpus showed unplaced or shaped differently. A quoted
# element after a pair stays an element of the composer's list, and
# `quietly` covers the whole comma list, as in Rakudo.

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
is placed(Q|class A { method Str() handles 'uc' { "a" } }|),
    'Class@0-45 Name@6-7 Block@8-45 Blockoid@8-45 Method@10-43 Name@17-20 Signature@21-21 Trait::Handles@23-35 QuotedString@31-35 StrLiteral@32-34 Blockoid@36-43 QuotedString@38-41 StrLiteral@39-40',
    'a quoted handles on a method';
is placed(Q|anon Str sub {};|),
    'Sub@0-15 Type::Simple@5-8 Name@5-8 Blockoid@13-15',
    'the return type of an anon sub';
is placed(Q|{ ;  ; };|),
    'Block@0-8 Blockoid@0-8',
    'a block of empty statements';
is placed(Q|sub foo ($foo where { $_ eq "foo"}) { $foo }|),
    'Sub@0-44 Name@4-7 Signature@9-34 Parameter@9-34 ParameterTarget::Var@9-13 VarDeclaration::Simple@9-13 Block@20-34 Blockoid@20-34 ApplyInfix@22-33 Infix@25-27 ArgList@22-33 Var::Lexical@22-24 QuotedString@28-33 StrLiteral@29-32 Blockoid@36-44 Var::Lexical@38-42',
    'the where block of a parameter';
is placed(Q|sub xz($x = * <=> *) { }|),
    'Sub@0-24 Name@4-6 Signature@7-19 Parameter@7-19 ParameterTarget::Var@7-9 VarDeclaration::Simple@7-9 ApplyInfix@12-19 Infix@14-17 ArgList@12-19 Blockoid@21-24',
    'a parameter default of whatevers only';
is placed(Q|my $h = { a => 1, "b", 2 }|),
    'VarDeclaration::Simple@0-26 Initializer::Assign@6-26 Circumfix::HashComposer@8-26 ApplyListInfix@10-24 Infix@21-22 FatArrow@10-16 IntLiteral@15-16 QuotedString@18-21 StrLiteral@19-20 IntLiteral@23-24',
    'a quoted element after a pair in a hash composer';
is placed(Q|say quietly 1 ~~ /1/, 'msg'|),
    'Call::Name::WithoutParentheses@0-27 Name@0-3 ArgList@4-27 ApplyListInfix@12-27 Infix@20-21 ApplyInfix@12-20 Infix@14-16 ArgList@12-20 IntLiteral@12-13 QuotedRegex@17-20 Regex::Literal@18-19 QuotedString@22-27 StrLiteral@23-26',
    'quietly over a comma list';

use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11, for a fourth set of
# constructs the roast corpus showed unplaced. A regex declaration's
# signature also visits the implicit parameters a method's does.

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
is placed(Q[say <a b c d>.[*-1]]),
    'Call::Name::WithoutParentheses@0-19 Name@0-3 ArgList@4-19 ApplyPostfix@4-19 QuotedString@4-13 StrLiteral@5-12 Postcircumfix::ArrayIndex@14-19 SemiList@15-18 ApplyInfix@15-18 Infix@16-17 ArgList@15-18 IntLiteral@17-18',
    'a dotted subscript on a word list';
is placed(Q[class P { method x () is rw { self[0] } }]),
    'Class@0-41 Name@6-7 Block@8-41 Blockoid@8-41 Method@10-39 Name@17-18 Signature@20-20 Trait::Is@22-27 Name@25-27 Blockoid@28-39 ApplyPostfix@30-37 Term::Self@30-34 Postcircumfix::ArrayIndex@34-37 SemiList@35-36 IntLiteral@35-36',
    'a subscript on self';
is placed(Q[say flat(1, 2).hash.<a>]),
    'Call::Name::WithoutParentheses@0-23 Name@0-3 ArgList@4-23 ApplyPostfix@4-23 ApplyPostfix@4-19 Call::Name@4-14 Name@4-8 ArgList@9-13 IntLiteral@9-10 IntLiteral@12-13 Call::Method@15-19 Name@15-19 ArgList@19-19 Postcircumfix::LiteralHashIndex@20-23 QuotedString@21-22 StrLiteral@21-22',
    'a dotted literal hash index after a method call';
is placed(Q[my regex fishy2 { $<n> = (.*)shark }]),
    'RegexDeclaration@0-36 Name@9-15 Regex::Sequence@18-35 Regex::NamedCapture@18-29 Regex::CapturingGroup@25-29 Regex::QuantifiedAtom@26-28 Regex::CharClass::Any@26-27 Regex::Quantifier::ZeroOrMore@27-28 Regex::WithWhitespace@29-35 Regex::Literal@29-34',
    'a capture alias with blanks around its =';
is placed(Q[class A { method aie { } }; class B { has A $.bar handles q[aie] }]),
    'Class@0-26 Name@6-7 Block@8-26 Blockoid@8-26 Method@10-24 Name@17-20 Blockoid@21-24 Class@28-66 Name@34-35 Block@36-66 Blockoid@36-66 VarDeclaration::Simple@38-64 Type::Simple@42-43 Name@42-43 Trait::Handles@50-64 QuotedString@58-64 StrLiteral@60-63',
    'a handles trait with a quote construct';
is placed(Q[enum E <a>; BEGIN for E.enums { }]),
    'Type::Enum@0-10 Name@5-6 QuotedString@7-10 StrLiteral@8-9 ApplyPostfix@22-29 Type::Simple@22-23 Name@22-23 Call::Method@24-29 Name@24-29 ArgList@29-29 Block@30-33 Blockoid@30-33',
    'a statement-control statement under a phaser';

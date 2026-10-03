use Test;

# Enumerated character-class assertions (`<[a..z]>`, `<-alpha>`) in RakuAST,
# measured on rakudo 2026.09.

plan 20;

sub body($src) { $src.AST.statements.head.expression.body }

my $cc = body(Q|/<[a..z\w]-[q]>/|);
isa-ok $cc, RakuAST::Regex::Assertion::CharClass, '<[...]> is Assertion::CharClass';
isa-ok $cc, RakuAST::Regex::Assertion, 'which is an Assertion';
is $cc.elements.elems, 2, 'one element per +/- term';
my $first = $cc.elements[0];
isa-ok $first, RakuAST::Regex::CharClassElement::Enumeration, 'a [...] term is an Enumeration';
is-deeply $first.negated, False, 'the first term is positive';
isa-ok $first.elements[0], RakuAST::Regex::CharClassEnumerationElement::Range, 'a..z is a Range';
is ($first.elements[0].from, $first.elements[0].to), (97, 122), 'with codepoint bounds';
isa-ok $first.elements[1], RakuAST::Regex::CharClass::Word, '\w inside a class is CharClass::Word';
is-deeply $cc.elements[1].negated, True, '-[q] is negated';
is $cc.elements[1].elements[0].character, 'q', 'a Character holds its character';

my $rule = body(Q|/<-alpha>/|).elements[0];
isa-ok $rule, RakuAST::Regex::CharClassElement::Rule, '<-alpha> is a Rule element';
is $rule.name, 'alpha', 'with its name';
is-deeply $rule.negated, True, 'negated';
is body(Q|/<[\]\-]>/|).elements[0].elements.map(*.character).join, ']-',
  'escaped characters are Characters';
is body(Q|/<[\x41..\x5A]>/|).elements[0].elements[0].to, 90, 'escaped range bounds';

is RakuAST::Regex::CharClassEnumerationElement::Character.new("a").raku,
  "RakuAST::Regex::CharClassEnumerationElement::Character.new(\n  \"a\"\n)",
  'a Character renders on its own line';

is ~("hello" ~~ EVAL(Q|/<[a..z]-[aeiou]>+/|.AST)), 'h', 'a class difference';
is ~("x-y z" ~~ EVAL(Q|/<[\w\-]>+/|.AST)), 'x-y', 'an escaped - and \w';
is ~("ab1" ~~ EVAL(Q|/<-alpha>/|.AST)), '1', 'a negated rule';
is ~("cz" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::Assertion::CharClass.new(
  RakuAST::Regex::CharClassElement::Enumeration.new(elements => (
    RakuAST::Regex::CharClassEnumerationElement::Range.new(from => 97, to => 99),
    RakuAST::Regex::CharClassEnumerationElement::Character.new("z"))))))), 'c',
  'a hand-built class';

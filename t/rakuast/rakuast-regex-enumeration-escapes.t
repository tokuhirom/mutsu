use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 16;

my $body = Q|/<[\b \B \a \q]>/|.AST.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::Assertion::CharClass,
    'the escaped enumeration has a character-class assertion';
my @entries = $body.elements[0].elements;
is @entries.elems, 4, 'each escape remains a separate entry';
isa-ok @entries[0], RakuAST::Regex::CharClass::BackSpace,
    'backspace keeps its dedicated class';
is-deeply @entries[0].negated, False, 'lowercase backspace is positive';
isa-ok @entries[1], RakuAST::Regex::CharClass::BackSpace,
    'uppercase backspace keeps the same class';
is-deeply @entries[1].negated, True, 'uppercase backspace is negated';
is @entries[2].character, 'a', 'an unknown escaped letter is literal';
is @entries[3].character, 'q', 'another unknown escaped letter is literal';

is ~("\b" ~~ EVAL(Q|/<[\b]>/|.AST)), "\b", 'backspace matches after lowering';
is ~( 'a' ~~ EVAL(Q|/<[\B]>/|.AST)), 'a', 'negated backspace matches another character';
is ~( 'a' ~~ EVAL(Q|/<[\a]>/|.AST)), 'a', 'literal escaped a matches after lowering';
is ~( 'q' ~~ EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::CharClass.new(
        RakuAST::Regex::CharClassElement::Enumeration.new(
            elements => (RakuAST::Regex::CharClassEnumerationElement::Character.new('q'),)
        )
    )
))), 'q', 'a hand-built literal enumeration entry lowers';

my $commented = Q|/<[f] #`[comment] + [o]>/|.AST.statements[0].expression.body;
is $commented.elements.elems, 2, 'an embedded comment separates arithmetic terms';
is 'foo'.comb(EVAL(Q|/<[f] #`[comment] + [o]>/|.AST)).join,
    'foo', 'a class with an embedded comment lowers and matches';

my $negated = RakuAST::Regex::CharClass::BackSpace.new(:negated);
is-deeply $negated.negated, True, 'a hand-built backspace class accepts negation';
is ~( 'a' ~~ EVAL(RakuAST::QuotedRegex.new(
    body => $negated,
))), 'a', 'a hand-built negated backspace class lowers';

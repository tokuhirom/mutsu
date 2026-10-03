use Test;

# `<(` / `)>` and Unicode properties in RakuAST, measured on rakudo 2026.09.

plan 15;

sub body($src) { $src.AST.statements.head.expression.body }

my @terms = body(Q|/a <( b )> c/|).terms;
isa-ok @terms[1].regex, RakuAST::Regex::MatchFrom, '<( is MatchFrom';
isa-ok @terms[3].regex, RakuAST::Regex::MatchTo, ')> is MatchTo';
isa-ok RakuAST::Regex::MatchFrom.new, RakuAST::Regex::Atom, 'MatchFrom is an Atom';
is ("xabc" ~~ EVAL(Q|/a <( b )> c/|.AST)).Str, 'b', 'the markers survive the round trip';

my $lu = body(Q|/<:Lu>/|);
isa-ok $lu, RakuAST::Regex::Assertion::CharClass, '<:Lu> is a CharClass assertion';
my $p = $lu.elements[0];
isa-ok $p, RakuAST::Regex::CharClassElement::Property, 'holding a Property';
is $p.property, 'Lu', 'named Lu';
is-deeply ($p.negated, $p.inverted), (False, False), 'neither negated nor inverted';

my $both = body(Q|/<-:!Lu>/|).elements[0];
is-deeply ($both.negated, $both.inverted), (True, True), '<-:!Lu> is negated and inverted';

my @mixed = body(Q|/<[x] + :Lu - :N>/|).elements;
is @mixed.map(*.^name.split('::').tail).join(','), 'Enumeration,Property,Property',
    'a property may follow an enumeration';
is-deeply @mixed[2].negated, True, 'with its own sign';

is RakuAST::Regex::CharClassElement::Property.new(:property<L>, :inverted).inverted, True,
    'Property.new takes inverted';
is ("aBc1D" ~~ EVAL(Q|/<:Lu>+/|.AST)).Str, 'B', '<:Lu>+ survives the round trip';
is ("aB1;" ~~ EVAL(Q|/<:L+:N>+/|.AST)).Str, 'aB1', 'a property union survives it';
is ("Ab" ~~ EVAL(Q|/<:!Lu>/|.AST)).Str, 'b', 'an inverted property survives it';

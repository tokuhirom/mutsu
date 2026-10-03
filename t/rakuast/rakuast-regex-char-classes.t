use Test;

# Regex character-class atoms (`\w`, `.`, `\x41`) and literals in RakuAST,
# measured on rakudo 2026.09.

plan 24;

sub body($src) { $src.AST.statements.head.expression.body }

# Read direction: the backslash classes and their negations.
my @classes = body(Q|/\w\W\s\d\n\h\v\t\e\f\r\0/|).terms;
is @classes.map(*.^name).join(' '),
  <Word Word Space Digit Newline HorizontalSpace VerticalSpace Tab Escape FormFeed CarriageReturn Nul>
    .map({ "RakuAST::Regex::CharClass::$_" }).join(' '),
  'each backslash class is its own node';
is-deeply @classes[0].negated, False, '\w is not negated';
is-deeply @classes[1].negated, True, '\W is negated';
isa-ok @classes[0], RakuAST::Regex::CharClass::Negatable, 'Word is Negatable';
nok @classes[11] ~~ RakuAST::Regex::CharClass::Negatable, 'Nul is not';

isa-ok body(Q|/./|), RakuAST::Regex::CharClass::Any, '. is CharClass::Any';

# Codepoint escapes keep only the characters they denote.
my $x = body(Q|/\x[41,42]/|);
isa-ok $x, RakuAST::Regex::CharClass::Specified, '\x[41,42] is Specified';
is $x.characters, 'AB', 'with the characters it denotes';
is body(Q|/\c[LATIN SMALL LETTER A]/|).characters, 'a', '\c[NAME]';
is body(Q|/\o101/|).characters, 'A', '\o101';
is-deeply body(Q|/\X41/|).negated, True, '\X41 is negated';

# Literals hold word characters; an escaped metacharacter joins them.
my $lit = body(Q|/a\#b/|);
is $lit.terms.elems, 1, 'a\#b is one term';
is $lit.terms[0].text, 'a#b', 'the literal a#b';
my $dot = body(Q|/a.b/|);
is $dot.terms.map(*.^name).join(' '),
  'RakuAST::Regex::Literal RakuAST::Regex::CharClass::Any RakuAST::Regex::Literal',
  'a.b is a literal, Any, a literal';
is body(Q|/\#ab+/|).terms.map(*.^name).join(' '),
  'RakuAST::Regex::Literal RakuAST::Regex::QuantifiedAtom',
  'a quantified literal splits off its last character without a nested Sequence';

# Rendering.
is RakuAST::Regex::CharClass::Specified.new(:characters<A>, :negated).raku,
  "RakuAST::Regex::CharClass::Specified.new(\n  negated    => True,\n  characters => \"A\"\n)",
  'Specified renders negated before characters';
is RakuAST::Regex::CharClass::Word.new.raku, 'RakuAST::Regex::CharClass::Word.new',
  'an unnegated class renders bare';

# Write direction: EVAL of the round-tripped and hand-built trees.
ok "a5" ~~ EVAL(Q|/a\d/|.AST), 'a\d matches a5';
nok "ad" ~~ EVAL(Q|/a\d/|.AST), 'and not ad';
ok "a.b#c" ~~ EVAL(Q|/a.b\#c/|.AST), '. and an escaped #';
nok "a.bxc" ~~ EVAL(Q|/a.b\#c/|.AST), 'the escaped # is literal';
is ~("x A\tB" ~~ EVAL(Q|/\w\s\x41\t\c[LATIN CAPITAL LETTER B]/|.AST)), "x A\tB",
  'classes and codepoint escapes match';
ok "ab" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::Sequence.new(
  RakuAST::Regex::Literal.new("a"), RakuAST::Regex::CharClass::Word.new))),
  'a hand-built Word class';
nok "a b" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::Sequence.new(
  RakuAST::Regex::Literal.new("a"), RakuAST::Regex::CharClass::Space.new(:negated)))),
  'a hand-built negated Space class';

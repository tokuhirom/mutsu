use Test;

# Regex quantifier ranges, backtracking modifiers and separators in RakuAST,
# measured on rakudo 2026.09.

plan 28;

sub body($src) { $src.AST.statements.head.expression.body }

# Ranges keep only the bounds and exclusions that were written.
my $exact = body(Q|/a**3/|).quantifier;
isa-ok $exact, RakuAST::Regex::Quantifier::Range, '**3 is a Range';
isa-ok $exact, RakuAST::Regex::Quantifier, 'which is a Quantifier';
is $exact.min, 3, 'min 3';
is $exact.max, 3, 'max 3';
my $open = body(Q|/a**2..*/|).quantifier;
is $open.min, 2, '**2..* has min 2';
nok $open.max.defined, 'and no max';
my $upto = body(Q|/a**^3/|).quantifier;
nok $upto.min.defined, '**^3 has no min';
is-deeply $upto.excludes-max, True, 'and excludes its max';
my $both = body(Q|/a**0^..^5/|).quantifier;
is-deeply ($both.min, $both.excludes-min, $both.max, $both.excludes-max), (0, True, 5, True),
  '**0^..^5 keeps both exclusions';
isa-ok body(Q|/a ** 2/|).atom, RakuAST::Regex::WithWhitespace,
  'whitespace before ** wraps the atom';

# Backtracking modifiers are type objects.
my @mods = body(Q|/a+? b*! c?: d**?2/|).terms;
ok @mods[0].regex.quantifier.backtrack === RakuAST::Regex::Backtrack::Frugal, '+? is Frugal';
ok @mods[1].regex.quantifier.backtrack === RakuAST::Regex::Backtrack::Greedy, '*! is Greedy';
ok @mods[2].regex.quantifier.backtrack === RakuAST::Regex::Backtrack::Ratchet, '?: is Ratchet';
ok @mods[3].quantifier.backtrack === RakuAST::Regex::Backtrack::Frugal,
  'a range takes its modifier after the **';
ok body(Q|/a+/|).quantifier.backtrack === RakuAST::Regex::Backtrack,
  'no modifier answers the Backtrack type object';

# Separators.
my $sep = body(Q|/a+%","/|);
isa-ok $sep.separator, RakuAST::Regex::Quote, '% takes the separator atom';
is-deeply $sep.trailing-separator, False, '% is not trailing';
is-deeply body(Q|/a*%%","/|).trailing-separator, True, '%% is trailing';
ok body(Q|/a+/|).separator === RakuAST::Regex::Term, 'no separator answers the Term type object';
my @spaced = body(Q|/a+ % "," b/|).terms;
isa-ok @spaced[0], RakuAST::Regex::WithWhitespace,
  'whitespace before % wraps the quantified atom';
isa-ok @spaced[0].regex.separator, RakuAST::Regex::WithWhitespace,
  'whitespace after the separator wraps the separator';

# Rendering.
is RakuAST::Regex::Quantifier::Range.new(:min(2), :max(3), :backtrack(RakuAST::Regex::Backtrack::Frugal)).raku,
  "RakuAST::Regex::Quantifier::Range.new(\n  min       => 2,\n  max       => 3,\n  backtrack => RakuAST::Regex::Backtrack::Frugal\n)",
  'a Range renders its bounds and modifier';

# Write direction.
is ~("aaaa" ~~ EVAL(Q|/a**2..3/|.AST)), 'aaa', 'a range matches up to its max';
is ~("aaaa" ~~ EVAL(Q|/a+?/|.AST)), 'a', 'a frugal quantifier matches the least';
is ~("a,b,c" ~~ EVAL(Q|/\w+ % ','/|.AST)), 'a,b,c', 'a separated list';
is ~("a,b,c," ~~ EVAL(Q|/\w+ %% ','/|.AST)), 'a,b,c,', 'a trailing separator';
is ~("aaa" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::QuantifiedAtom.new(
  atom => RakuAST::Regex::Literal.new("a"),
  quantifier => RakuAST::Regex::Quantifier::Range.new(:min(2), :max(2)))))), 'aa',
  'a hand-built Range';
is ~("x-y-z" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::QuantifiedAtom.new(
  atom => RakuAST::Regex::CharClass::Word.new,
  quantifier => RakuAST::Regex::Quantifier::OneOrMore.new,
  separator => RakuAST::Regex::Literal.new("-"))))), 'x-y-z',
  'a hand-built separator';

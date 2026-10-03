use Test;

plan 14;

sub body($src) { $src.AST.statements.head.expression.body }

my $b = body(Q|/ :i ^foo /|);
isa-ok $b.terms[0], RakuAST::Regex::InternalModifier::IgnoreCase, ':i is an IgnoreCase node';
isa-ok $b.terms[0], RakuAST::Regex::InternalModifier, 'which is an InternalModifier';
isa-ok $b.terms[1], RakuAST::Regex::Anchor::BeginningOfString, '^ after a modifier is still the start anchor';
is $b.terms[0].modifier, 'i', 'the short spelling';
is-deeply $b.terms[0].negated, False, 'not negated';

my $long = body(Q|/ :ignorecase a :ignoremark b /|);
is $long.terms[0].modifier, 'ignorecase', 'the long spelling is kept';
isa-ok $long.terms[2], RakuAST::Regex::InternalModifier::IgnoreMark, ':ignoremark is IgnoreMark';
is-deeply body(Q|/ :!i a /|).terms[0].negated, True, ':!i is negated';
is RakuAST::Regex::InternalModifier::IgnoreMark.new(:negated, modifier => "ignoremark").raku,
  qq:to/END/.chomp, 'renders its non-default fields';
RakuAST::Regex::InternalModifier::IgnoreMark.new(
  modifier => "ignoremark",
  negated  => True
)
END

ok "FOObar" ~~ EVAL(Q|/ :i ^foo /|.AST), 'an EVALed :i regex ignores case';
nok "xFOObar" ~~ EVAL(Q|/ :i ^foo /|.AST), 'and keeps its anchor';
ok "aB" ~~ EVAL(Q|/ a [ :i b ] /|.AST), 'a modifier scoped to a group';
nok "AB" ~~ EVAL(Q|/ a [ :i b ] /|.AST), 'does not leak out of the group';
ok "a" ~~ EVAL(RakuAST::QuotedRegex.new(body => RakuAST::Regex::Sequence.new(
  RakuAST::Regex::InternalModifier::IgnoreCase.new, RakuAST::Regex::Literal.new("A")))),
  'a hand-built IgnoreCase';

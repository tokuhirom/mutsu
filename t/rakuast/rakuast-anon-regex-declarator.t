use Test;

# An anonymous `regex { }` / `token { }` / `rule { }` term in RakuAST, measured
# on rakudo 2026.09: the same declaration node as the named form, with no
# `name` (its accessor answers the `Name` type object) and an optional
# `signature`. EVAL of the tree builds the regex value again.

plan 13;

sub term($src) { Q[my $r = ] ~ $src ~ Q[;] andthen .AST.statements.head.expression.initializer.expression }

my $regex = term('regex { a <b> }');
isa-ok $regex, RakuAST::RegexDeclaration, 'an anonymous regex term';
nok $regex.name.defined, 'it has no name';
isa-ok term('token { a }'), RakuAST::TokenDeclaration, 'an anonymous token';
isa-ok term('rule { a b }'), RakuAST::RuleDeclaration, 'an anonymous rule';
is term('token ($x, :$y) { a }').signature.parameters.elems, 2, 'a signature is kept';

sub run($src) { EVAL($src.AST) }
is run(Q[my $r = token { \d+ }; ~("ab12" ~~ $r)]), '12', 'a token term matches';
is run(Q[my $r = rule { a b }; ~("a   b" ~~ $r)]), 'a   b', 'a rule term takes implicit whitespace';
nok run(Q[my $r = token { a b }; so "a b" ~~ $r]), 'a token term ratchets and ignores spaces';
is run(Q[my $r = regex { a | ab }; ~("ab" ~~ /<$r>/)]), 'ab', 'a regex term backtracks into a match';
is run(Q[my $r = token ($c) { x+ }; ~("xx" ~~ $r)]), 'xx', 'a token term with a signature round-trips';

my $built = RakuAST::TokenDeclaration.new(body => RakuAST::Regex::Literal.new("ab"));
nok $built.name.defined, 'a hand-built anonymous declaration has no name';
is ~("xab" ~~ EVAL($built)), 'ab', 'and evaluates to a regex';
isa-ok EVAL($built), Regex, 'which is a Regex';

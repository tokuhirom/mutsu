use Test;
use MONKEY-SEE-NO-EVAL;

# A grammar built at run time through the MOP (#10633):
# `Metamodel::GrammarHOW.new_type` composes as a Grammar, `^add_method` with a
# regex value installs a grammar rule `.parse` and `<x>` resolve, and
# `Code.set_name` renames a regex in place.

plan 13;

my $grmr := Metamodel::GrammarHOW.new_type(:name<MyNewGrammar>);
my $body = EVAL 'regex { a | b }';
$grmr.^add_method('x', $body);
$body.set_name('x');
is $body.name, 'x', 'Regex.name reports the set name';
my $top = EVAL 'regex { <x> }';
$grmr.^add_method('TOP', $top);
$grmr.^compose;

ok $grmr ~~ Grammar, 'a GrammarHOW.new_type type is a Grammar';
is $grmr.^name, 'MyNewGrammar', 'the type keeps its name';
ok $grmr.parse('a'), 'parse resolves the added TOP rule and its <x> subrule';
my $m = $grmr.parse('b');
is ~$m, 'b', 'the match covers the input';
is ~$m<x>, 'b', 'the <x> subrule captures under its rule name';
nok $grmr.parse('c'), 'a non-matching input fails';

# An alias sees the rename: a regex is one code object.
my $r = regex { foo };
my $alias = $r;
is $r.name, '', 'an anonymous regex has an empty name';
$r.set_name('renamed');
is $alias.name, 'renamed', 'the rename is seen through an alias';

# Declarator terms other than `regex`, built without EVAL.
my $g2 := Metamodel::GrammarHOW.new_type(:name<TokenGrammar>);
$g2.^add_method('TOP', token { <digit>+ <sep> <digit>+ });
$g2.^add_method('sep', rule { '-' });
$g2.^compose;
ok $g2.parse('12-34'), 'token and rule values become rules';
is ~$g2.parse('12-34')<sep>, '-', 'the rule value is reachable as a subrule';

# An explicit parent replaces the default Grammar parent.
grammar Base { token TOP { <word> }; token word { \w+ } }
my $g3 := Metamodel::GrammarHOW.new_type(:name<Derived>);
$g3.^add_parent(Base);
$g3.^add_method('word', regex { \d+ });
$g3.^compose;
ok $g3 ~~ Base, 'an added parent is honored';
ok $g3.parse('42') && !$g3.parse('ab'), 'the added rule overrides the parent one';

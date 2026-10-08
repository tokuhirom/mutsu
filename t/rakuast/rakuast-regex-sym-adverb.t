use v6;
use MONKEY-SEE-NO-EVAL;
use Test;

# `<value:sym<number>>`: rakudo keeps the `:sym<number>` adverb as a
# `ColonPair::Value` on the assertion's name and renders no `args`.
# Expected shapes measured on Rakudo 2026.09.

plan 10;

my $code = 'grammar G { token value:sym<number> { \d+ };  token TOP { <value:sym<number>> } }';

sub assertion(Str $src) {
    my $top = $src.AST.statements[0].expression.body.body.statement-list.statements[*-1].expression;
    $top.body.regex;
}

my $a = assertion($code);
isa-ok $a, RakuAST::Regex::Assertion::Named, 'the assertion is a plain Named one';
is $a.name.colonpairs.elems, 1, 'the name carries one colonpair';
my $pair = $a.name.colonpairs[0];
isa-ok $pair, RakuAST::ColonPair::Value, 'a value colonpair';
is $pair.key, 'sym', 'keyed sym';
is $pair.value.segments[0].value, 'number', 'with the adverb text';
ok $a.capturing, 'it still captures';

# --- the round trip computes what the grammar does --------------------------

# A second name: rakudo declares the grammar of an `.AST` at `.AST` time.
my $ast = $code.subst('grammar G', 'grammar G2').AST;
my $g = EVAL $ast;
my $m = $g.parse('12');
ok $m, 'the grammar parses through the candidate';
is $m<value:sym<number>>.Str, '12', 'the capture is keyed by the long name';

# --- the alias spelling ------------------------------------------------------

my $alias = 'grammar H { token value:sym<n> { \d+ }; token TOP { <x=value:sym<n>> } }'.AST;
my $h = EVAL $alias;
ok $h.parse('7'), 'an aliased candidate call parses';
is $h.parse('7')<x>.Str, '7', 'under the alias';

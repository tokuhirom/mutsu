use Test;

# More regex forms the source-tree parser models, as rakudo 2026.09 renders them:
#
# - `a ** {2}` is a `QuantifiedAtom` whose quantifier is a `Quantifier::BlockRange` over a
#   `Block` (`**?{2}` adds the `backtrack`);
# - `a:`, `a:!`, `a:?` is a `BacktrackModifiedAtom` over the atom;
# - `<:Script<Latin>>` and `<:Nv(1)>` are a `CharClassElement::Property` with a `predicate`
#   (a words `QuotedString`, or a parenthesised expression);
# - `"x $y z"` is a `Regex::Quote` of a `QuotedString` with the variable as a segment;
# - `<-restricted +name-sep>` is a class of two `CharClassElement::Rule`s, the second named
#   with a hyphen;
# - the adverbs of `m:x(2)/a/` are colonpairs on its `QuotedRegex`.
#
# The round trip is the parsed program. The tree part of this file also passes under `raku`;
# the round trip part is mutsu's.

plan 38;

sub body($src) { $src.AST.statements.head.expression.body }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- block ranges
{
    my $q = body(Q[/a **{2}/]);
    isa-ok $q, RakuAST::Regex::QuantifiedAtom, '`a **{2}` is a quantified atom';
    isa-ok $q.quantifier, RakuAST::Regex::Quantifier::BlockRange, 'with a block range';
    isa-ok $q.quantifier.block, RakuAST::Block, 'over a block';
    is $q.quantifier.block.body.statement-list.statements[0].expression.value, 2, 'holding the expression';
    is body(Q[/a **?{2}/]).quantifier.backtrack.^name, 'RakuAST::Regex::Backtrack::Frugal', 'a modifier is kept';
    isa-ok body(Q[/a ** {1..3}/]).quantifier.block.body.statement-list.statements[0].expression,
        RakuAST::ApplyInfix, 'a range in the block';
}

# --- backtrack-modified atoms
{
    my $a = body(Q[/a:/]);
    isa-ok $a, RakuAST::Regex::BacktrackModifiedAtom, '`a:` is a backtrack-modified atom';
    isa-ok $a.atom, RakuAST::Regex::Literal, 'over its atom';
    is $a.backtrack.^name, 'RakuAST::Regex::Backtrack::Ratchet', 'a bare `:` is a ratchet';
    is body(Q[/a:!/]).backtrack.^name, 'RakuAST::Regex::Backtrack::Greedy', '`:!` is greedy';
    is body(Q[/a:?/]).backtrack.^name, 'RakuAST::Regex::Backtrack::Frugal', '`:?` is frugal';
    isa-ok body(Q[/<digit>:/]).atom, RakuAST::Regex::Assertion::Named, 'a subrule can be modified';
}

# --- property predicates
{
    my $p = body(Q[/<:Script<Latin>>/]).elements[0];
    isa-ok $p, RakuAST::Regex::CharClassElement::Property, '`<:Script<Latin>>` is a property';
    is $p.property, 'Script', 'with its name';
    isa-ok $p.predicate, RakuAST::QuotedString, 'a words predicate is a quoted string';
    is $p.predicate.segments[0].value, 'Latin', 'with the word';
    my $n = body(Q[/<:Nv(1)>/]).elements[0];
    isa-ok $n.predicate, RakuAST::Circumfix::Parentheses, '`<:Nv(1)>` has a parenthesised predicate';
    isa-ok $n.predicate.semilist.statements[0].expression, RakuAST::IntLiteral, 'holding the number';
    isa-ok body(Q[/<:Line_Break("ID")>/]).elements[0].predicate.semilist.statements[0].expression,
        RakuAST::QuotedString, 'or a string';
}

# --- interpolating quotes
{
    my $q = ('my $x; ' ~ Q[/"x $x z"/]).AST.statements[1].expression.body;
    isa-ok $q, RakuAST::Regex::Quote, 'an interpolating quote is a quote';
    is $q.quoted.segments.elems, 3, 'of three segments';
    isa-ok $q.quoted.segments[1], RakuAST::Var::Lexical, 'the variable in the middle';
    isa-ok body(Q[/"a{ 1 }b"/]).quoted.segments[1], RakuAST::Block, 'or a code block';
}

# --- hyphenated names in a class
{
    my $c = body(Q[/<-restricted +name-sep>/]);
    is $c.elements.elems, 2, 'two class elements';
    is $c.elements[0].negated, True, 'the first negated';
    is $c.elements[1].name, 'name-sep', 'the second named with a hyphen';
}

# --- adverbs with arguments on a match
{
    my $m = Q[m:x(2)/a/].AST.statements.head.expression;
    isa-ok $m, RakuAST::QuotedRegex, '`m:x(2)/a/` is a quoted regex';
    isa-ok $m.adverbs[0], RakuAST::ColonPair::Value, 'with the adverb as a colonpair';
    is $m.adverbs[0].key, 'x', 'named `x`';
}

# --- the round trip is the parsed program
same Q[~("aaaa" ~~ /a ** {2}/)], 'aa', 'a block range';
same Q[my $n = 3; ~("aaaaa" ~~ /a ** {$n}/)], 'aaa', 'a block range over a variable';
same Q[~("aaaa" ~~ /a ** {1..3}/)], 'aaa', 'a block range of a range';
same Q[~("aab" ~~ /a+: b/)], 'aab', 'a possessive quantifier then an atom';
same Q[~("abc" ~~ /<alpha>: bc/)], 'abc', 'a modified subrule';
same Q[~("Δx" ~~ /<:Script<Greek>>/)], 'Δ', 'a property with a words predicate';
same Q[my $s = "b"; ~("abc" ~~ /a "$s" c/)], 'abc', 'an interpolating quote';
same Q[grammar G { token x { <-restricted +name-sep>+ }; token restricted { <[ : < > ( ) ]> }; token name-sep { '::' } }; ~G.subparse('Foo::Bar', :rule<x>)], 'Foo::Bar', 'a hyphenated name in a class';
same Q[~("aaaa" ~~ m:x(2)/a/)], 'a a', 'a match with an argument adverb';

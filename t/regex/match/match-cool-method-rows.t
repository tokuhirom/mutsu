use Test;

# ADR-11276 §9.96: a `Match` answers a name it does not declare on the Match
# itself, never on its matched string. `Cool`'s numeric methods and coercions
# read the matched text (`Match.Numeric` is `self.Str.Numeric`), `Any`'s list
# methods read the positional captures, and a name Rakudo's `Match` lacks is
# `X::Method::NotFound`. The answers below are Rakudo's.

plan 23;

my $n = "x 9 y" ~~ /\d/;
is $n.sqrt, 3, 'Cool.sqrt numifies the matched text';
is $n.abs, 9, 'Cool.abs';
is $n.is-prime, False, 'Cool.is-prime';
is $n.log10.round(0.001), 0.954, 'Cool.log10';
is-deeply $n.Real, 9, 'Cool.Real';
is-deeply $n.Rat, 9.0, 'Cool.Rat';
is-deeply ("1/4" ~~ /.+/).FatRat, FatRat.new(1, 4), 'Cool.FatRat';
is $n.Version, v9, 'Cool.Version';
throws-like { ("x" ~~ /x/).sqrt }, X::Str::Numeric, 'a non-numeric text fails';
throws-like { ("x" ~~ /x/).Rat }, X::Str::Numeric, '... as Cool.Rat does';
isa-ok $n.Failure, Failure, 'Cool.Failure';
is ("LATIN SMALL LETTER A" ~~ /.+/).uniparse, 'a', 'Cool.uniparse';
isa-ok ("say 1" ~~ /.+/).AST, RakuAST::StatementList, 'Cool.AST parses the text';

my $m = "12 ab" ~~ /(\d+)\s(\w+)/;
is $m.end, 1, 'Any.end counts the positional captures';
is ~$m.head, '12', 'Any.head is the first positional capture';
is $m.pairup.elems, 1, 'Any.pairup pairs the positional captures';
is $m.tree.WHAT, Match, 'Any.tree is the match';
is $m.list.elems, 2, 'Capture.list';
ok $m.Capture === $m, 'Capture.Capture is the match';
throws-like { $m.naive-word-wrapper }, X::Method::NotFound,
    'a Str-only method is not a method of a Match';
throws-like { $m.tc-first-only }, X::Method::NotFound,
    'nor is a name nobody declares';

grammar G { token TOP { <a> ' ' <b> }; token a { \d+ }; token b { \w+ } }
my $p = G.parse("12 ab");
is $p.end, -1, 'a grammar match has no positional captures';
throws-like { $p.sqrt }, X::Str::Numeric, '... and its text numifies as a Str';

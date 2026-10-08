use Test;

# Regex declarations whose body the source tree could not read, so the
# declaration refused to cross the RakuAST boundary. Shapes measured on rakudo
# 2026.09.
#
#   'a' #`(doc) 'b'        comments are whitespace: each atom is WithWhitespace
#   'a' : 'b'              BacktrackModifiedAtom, the atom wrapped in WithWhitespace
#   '(' ~ ')' <k>+ % ','   the expression of a `~` takes a quantifier and separator
#   <|w>                   the same node as <.wb>
#   <?before $<x>=a+>      a capture inside a lookahead
#   <\x[2B]>-style classes round-trip and still match

plan 19;

sub decl($body) { Q[my token t { ] ~ $body ~ Q[ }] }
sub tree($body) { decl($body).AST.gist }

# Read direction: the node classes of the body.
my $wb = tree(Q[<|w>]);
like $wb, /'Assertion::Named.new(' \s+ 'name => RakuAST::Name.from-identifier("wb")' \s+ ')'/,
    '<|w> is the named assertion wb, not capturing';
unlike $wb, /capturing/, 'with no capturing field';
like tree(Q['a' #`(doc) 'b']), /'WithWhitespace' .* 'StrLiteral.new("a")' .* 'WithWhitespace' .* 'StrLiteral.new("b")'/,
    'an embedded comment separates two atoms';
like tree("'a' # line\n 'b'"), /'WithWhitespace' .* 'StrLiteral.new("a")' .* 'WithWhitespace' .* 'StrLiteral.new("b")'/,
    'a line comment separates two atoms';
my $backtrack = tree(Q['a' : 'b']);
like $backtrack, /'BacktrackModifiedAtom.new('/, 'a spaced `:` is a backtrack modifier';
like $backtrack, /'backtrack => RakuAST::Regex::Backtrack::Ratchet'/, 'a ratchet one';
like tree(Q['a' :! 'b']), /'Backtrack::Greedy'/, 'a spaced `:!` is a greedy one';
my $tilde = tree(Q['(' ~ ')' <k>+ % ',']);
like $tilde, /'Regex::Nested.new(' .* 'QuantifiedAtom.new(' .* 'OneOrMore' .* 'separator'/,
    'a tilde construct whose expression is quantified and separated';
like tree(Q[<?before $<sp>=' '+>]), /'Lookahead.new(' .* 'RegexArg' .* 'NamedCapture.new(' .* 'name  => "sp"' .* 'OneOrMore'/,
    'a capture inside a lookahead';
like tree(Q[<!{ 1 ~~ / <["']>? / }>]), /'PredicateBlock.new(' .* 'negated => True' .* 'QuotedRegex.new('/,
    'a quote in a nested regex literal does not open a string';
like tree(Q[<.digit> <?{ # It's a } comment
 True }>]), /'PredicateBlock.new('/, 'a comment in a code block may hold quotes and braces';

# Write and semantics.
sub run($src) { EVAL($src.AST) }
is run(Q[my token t { <[x]> : 'b' }; ~("xb" ~~ /<t>/)]), 'xb', 'a spaced `:` still matches';
is run(Q[my token k { <[a..z]> }; my token t { '(' ~ ')' <k>+ % ',' }; ~("(a,b)" ~~ /<t>/)]),
    '(a,b)', 'a quantified tilde expression matches through the round trip';
is run(Q[my token t { 'ab' <|w> }; so "ab c" ~~ /<t>/]), True, '<|w> holds at a word end';
is run(Q[my token t { 'ab' <|w> }; so "abc" ~~ /<t>/]), False, 'and fails inside a word';
is run(Q[my token t { 'a' #`(skip) 'b' }; ~("ab" ~~ /<t>/)]), 'ab', 'an embedded comment is ignored';
is run(Q[my token t { <?before $<sp>=' '+> ' ' }; ~("  x" ~~ /<t>/)]), ' ',
    'a capture inside a lookahead matches';
# `\x[..]` inside a character class must not close it early.
is run(Q[so "+4" ~~ /<[\x[2B]]> [a|4]/]), True, 'a \x[..] escape in a class before an alternation';
is run(Q[so "+" ~~ /<[\x[2B] a]>/]), True, 'a \x[..] escape followed by another class item';

done-testing;

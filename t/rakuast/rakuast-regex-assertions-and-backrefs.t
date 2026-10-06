use Test;

# Regex constructs the source-tree parser models, as rakudo 2026.09 renders them:
#
# - `<?name>` / `<!name>` / `<?.name>` / `<?[x]>` is an `Assertion::Lookahead` over a
#   named assertion or a character class (`<?before ...>` is the same node over a
#   `Named::RegexArg`), also when aliased (`$<a>=<?foo>`);
# - `< a b >` is a `Regex::Quote` of a `QuotedString` with the `words` processor;
# - `A ~ B C` is `Sequence(A, Nested(B, C))`;
# - `:my $x = 1;` is a `Regex::Statement` over the statement;
# - `$0` and `$<name>` are `Regex::BackReference::Positional` / `::Named`;
# - `<~~>` is `Assertion::Recurse`.
#
# The round trip is the parsed program. The tree part of this file also passes under
# `raku`; the round trip part is mutsu's.

plan 48;

sub body($src) { $src.AST.statements.head.expression.body }
sub terms($src) { body($src).terms }
sub same($src, $expected, $desc) {
    my $parsed = do { my @*LOG; EVAL($src) };
    my $round = do { my @*LOG; EVAL($src.AST) };
    is "$parsed|$round", "$expected|$expected", $desc;
}

# --- lookahead assertions
{
    my $l = body(Q[/<?alpha>/]);
    isa-ok $l, RakuAST::Regex::Assertion::Lookahead, '`<?alpha>` is a lookahead';
    is $l.negated, False, 'that is not negated';
    isa-ok $l.assertion, RakuAST::Regex::Assertion::Named, 'over a named assertion';
    is $l.assertion.name.canonicalize, 'alpha', 'with its name';
    is $l.assertion.capturing, True, 'which captures';
    my $n = body(Q[/<!ww>/]);
    is $n.negated, True, '`<!ww>` is negated';
    is body(Q[/<?.bee>/]).assertion.capturing, False, 'a dot-prefixed name does not capture';
    isa-ok body(Q[/<?[x]>/]).assertion, RakuAST::Regex::Assertion::CharClass, '`<?[x]>` is over a class';
    isa-ok body(Q[/<?foo(1)>/]).assertion, RakuAST::Regex::Assertion::Named::Args, 'a call with arguments';
    my $a = body(Q[/<!after <.space>>/]);
    is $a.negated, True, '`<!after ...>` is negated';
    isa-ok $a.assertion, RakuAST::Regex::Assertion::Named::RegexArg, 'over a regex argument';
    my $c = body(Q[/$<a>=<?foo>/]);
    isa-ok $c, RakuAST::Regex::NamedCapture, 'an aliased one is a named capture';
    isa-ok $c.regex, RakuAST::Regex::Assertion::Lookahead, 'of the lookahead';
}

# --- word lists
{
    my $q = body(Q[/< a b >/]);
    isa-ok $q, RakuAST::Regex::Quote, '`< a b >` is a quote';
    is $q.quoted.processors.join(','), 'words', 'with the `words` processor';
    is $q.quoted.segments[0].value, ' a b ', 'over the text as written';
}

# --- tilde
{
    my $s = body(Q[/"a"~"b"c/]);
    isa-ok $s, RakuAST::Regex::Sequence, '`A ~ B C` is a sequence';
    my $t = $s.terms[1];
    isa-ok $t, RakuAST::Regex::Nested, 'ending in a `Nested`';
    isa-ok $t.goal, RakuAST::Regex::Quote, 'whose goal is `B`';
    isa-ok $t.expr, RakuAST::Regex::Literal, 'and expression `C`';
    is $t.goal.quoted.segments[0].value, 'b', 'as written';
}

# --- statements
{
    my $s = terms(Q[/:my $x = 1;a/]);
    isa-ok $s[0], RakuAST::Regex::Statement, '`:my $x = 1;` is a regex statement';
    isa-ok $s[0].statement, RakuAST::Statement::Expression, 'holding a statement';
    isa-ok $s[0].statement.expression, RakuAST::VarDeclaration::Simple, 'that declares';
    isa-ok terms(Q[/:temp @*x;a/])[0].statement.expression, RakuAST::ApplyPrefix, '`:temp` is a prefix application';
}

# --- back-references and recursion
{
    my $p = terms(Q[/(a)$0/])[1];
    isa-ok $p, RakuAST::Regex::BackReference::Positional, '`$0` is a positional back-reference';
    is $p.index, 0, 'with its index';
    my $n = terms(Q[/$<c>=(a)$<c>/])[1];
    isa-ok $n, RakuAST::Regex::BackReference::Named, '`$<c>` is a named back-reference';
    is $n.name, 'c', 'with its name';
    isa-ok terms(Q[/"("~")"<~~>/])[1].expr, RakuAST::Regex::Assertion::Recurse, '`<~~>` is a recursion';
}

# --- in a declaration
{
    my $d = Q[token t { <?alpha> a }].AST.statements.head.expression.body;
    isa-ok $d.terms[0].regex, RakuAST::Regex::Assertion::Lookahead, 'a lookahead in a token';
    isa-ok Q[token t { '(' ~ ')' <b> }].AST.statements.head.expression.body.terms[1], RakuAST::Regex::Nested,
        'a tilde in a token';
}

# --- the round trip is the parsed program
same Q[~("abc" ~~ /<?alpha> ab/)], 'ab', 'a lookahead';
same Q[~("ab1" ~~ /<!digit> a/)], 'a', 'a negative lookahead';
same Q[~("ab" ~~ /<?[a]> a/)], 'a', 'a lookahead over a class';
same Q[~("a b" ~~ /a <!ww> ' ' b/)], 'a b', 'a negative word-character lookahead';
same Q[~("abab" ~~ /(ab) $0/)], 'abab', 'a positional back-reference';
same Q[~("xx" ~~ /$<c>=(x) $<c>/)], 'xx', 'a named back-reference';
same Q[~("c" ~~ /< a b c >/)], 'c', 'a word list';
same Q[~("(abc)" ~~ /'(' ~ ')' (\w+)/)], '(abc)', 'a tilde';
same Q[~("[x]" ~~ /'[' ~ ']' (.)/)], '[x]', 'a tilde without spaces';
same Q[~("ab" ~~ /:my $x = 'a'; $x b/)], 'ab', 'a statement';
same Q[my @seen; my $r = "ab" ~~ /:my $n = 7; a { @seen.push($n) } b/; "{~$r} @seen[]"], 'ab 7', 'a statement and a code block';
same Q[~("((a))" ~~ /'(' ~ ')' [ <~~> | a ]/)], '((a))', 'a recursion';
same Q[grammar G { token TOP { <?alpha> <w> }; token w { \w+ } }; ~G.parse("ab")], 'ab', 'a lookahead in a grammar';
same Q[grammar H { token TOP { '(' ~ ')' <w> }; token w { \w+ } }; ~H.parse("(ab)")], '(ab)', 'a tilde in a grammar';
same Q[grammar I { token TOP { <a> $<a> }; token a { x } }; ~I.parse("xx")], 'xx', 'a named back-reference in a grammar';
same Q[grammar J { rule TOP { '(' ~ ')' <w> }; token w { \w+ } }; ~J.parse("( ab )")], '( ab )', 'a tilde in a rule';

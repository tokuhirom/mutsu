use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST regex tree support (ADR-0088, issue #8033). The parser keeps a
# source-level static regex tree and the converter maps it to the same node
# shapes Rakudo exposes. Dynamic assertions and non-scalar interpolations
# remain explicit follow-up boundaries.

plan 71;

is Q[/a/].AST.gist, q:to/END/.chomp, 'a regex literal has a Literal body';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Literal.new("a")
        )
      )
    )
    END

is Q[m:i/test/].AST.gist, q:to/END/.chomp, 'm:i preserves match-immediately and its adverb';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          match-immediately => True,
          body              => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new("test")
          ),
          adverbs           => (
            RakuAST::ColonPair::True.new("i"),
          )
        )
      )
    )
    END

is Q[m:g/test/].AST.gist, q:to/END/.chomp, 'm:g keeps source adverb spelling';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          match-immediately => True,
          body              => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new("test")
          ),
          adverbs           => (
            RakuAST::ColonPair::True.new("g"),
          )
        )
      )
    )
    END

is Q[/test/].AST.gist, q:to/END/.chomp, 'a multi-character regex has a Sequence body';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new("test")
          )
        )
      )
    )
    END

is Q[/\d+/].AST.gist, q:to/END/.chomp, 'a digit class and quantifier remain structural';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::QuantifiedAtom.new(
            atom       => RakuAST::Regex::CharClass::Digit.new,
            quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
          )
        )
      )
    )
    END

is Q[/(a)/].AST.gist, q:to/END/.chomp, 'a capture group remains a CapturingGroup';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::CapturingGroup.new(
            RakuAST::Regex::Literal.new("a")
          )
        )
      )
    )
    END

is Q[/^a/].AST.gist, q:to/END/.chomp, 'a start-of-string anchor remains structural';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfString.new,
            RakuAST::Regex::Literal.new("a")
          )
        )
      )
    )
    END

is Q[/a$/].AST.gist, q:to/END/.chomp, 'an end-of-string anchor remains structural';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new("a"),
            RakuAST::Regex::Anchor::EndOfString.new
          )
        )
      )
    )
    END

is Q[/^^a/].AST.gist, q:to/END/.chomp, 'a start-of-line anchor remains structural';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Anchor::BeginningOfLine.new,
            RakuAST::Regex::Literal.new("a")
          )
        )
      )
    )
    END

is Q[/a$$/].AST.gist, q:to/END/.chomp, 'an end-of-line anchor remains structural';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::Literal.new("a"),
            RakuAST::Regex::Anchor::EndOfLine.new
          )
        )
      )
    )
    END

is Q[/foo <?before bar>/].AST.gist, q:to/END/.chomp, 'a positive lookahead keeps its named regex argument';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Lookahead.new(
              assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name      => RakuAST::Name.from-identifier("before"),
                regex-arg => RakuAST::Regex::Sequence.new(
                  RakuAST::Regex::Literal.new("bar")
                )
              )
            )
          )
        )
      )
    )
    END

is Q[/foo <!before bar>/].AST.gist, q:to/END/.chomp, 'a negative lookahead preserves negation';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Lookahead.new(
              negated   => True,
              assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                name      => RakuAST::Name.from-identifier("before"),
                regex-arg => RakuAST::Regex::Sequence.new(
                  RakuAST::Regex::Literal.new("bar")
                )
              )
            )
          )
        )
      )
    )
    END

is Q[/<?after foo> bar/].AST.gist, q:to/END/.chomp, 'a positive lookbehind keeps the after assertion';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Assertion::Lookahead.new(
                assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                  name      => RakuAST::Name.from-identifier("after"),
                  regex-arg => RakuAST::Regex::Sequence.new(
                    RakuAST::Regex::Literal.new("foo")
                  )
                )
              )
            ),
            RakuAST::Regex::Literal.new("bar")
          )
        )
      )
    )
    END

is Q[/<!after foo> bar/].AST.gist, q:to/END/.chomp, 'a negative lookbehind preserves negation';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Assertion::Lookahead.new(
                negated   => True,
                assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                  name      => RakuAST::Name.from-identifier("after"),
                  regex-arg => RakuAST::Regex::Sequence.new(
                    RakuAST::Regex::Literal.new("foo")
                  )
                )
              )
            ),
            RakuAST::Regex::Literal.new("bar")
          )
        )
      )
    )
    END

is Q[/foo <before bar>/].AST.gist, q:to/END/.chomp, 'an unprefixed lookahead keeps its named regex argument';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Named::RegexArg.new(
              name      => RakuAST::Name.from-identifier("before"),
              regex-arg => RakuAST::Regex::Sequence.new(
                RakuAST::Regex::Literal.new("bar")
              )
            )
          )
        )
      )
    )
    END

is Q[/foo <.before bar>/].AST.gist, q:to/END/.chomp, 'a dot-suppressed lookahead keeps the same model shape';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Named::RegexArg.new(
              name      => RakuAST::Name.from-identifier("before"),
              regex-arg => RakuAST::Regex::Sequence.new(
                RakuAST::Regex::Literal.new("bar")
              )
            )
          )
        )
      )
    )
    END

is Q[/foo <after bar>/].AST.gist, q:to/END/.chomp, 'an unprefixed lookbehind keeps its named regex argument';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Named::RegexArg.new(
              name      => RakuAST::Name.from-identifier("after"),
              regex-arg => RakuAST::Regex::Sequence.new(
                RakuAST::Regex::Literal.new("bar")
              )
            )
          )
        )
      )
    )
    END

is Q[/foo <.after bar>/].AST.gist, q:to/END/.chomp, 'a dot-suppressed lookbehind keeps the same model shape';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::Assertion::Named::RegexArg.new(
              name      => RakuAST::Name.from-identifier("after"),
              regex-arg => RakuAST::Regex::Sequence.new(
                RakuAST::Regex::Literal.new("bar")
              )
            )
          )
        )
      )
    )
    END

is Q[/foo <?before [<?before bar>]> bar/].AST.gist, q:to/END/.chomp, 'a nested static lookahead retains both assertion layers';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("foo")
            ),
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Assertion::Lookahead.new(
                assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                  name      => RakuAST::Name.from-identifier("before"),
                  regex-arg => RakuAST::Regex::Group.new(
                    RakuAST::Regex::Assertion::Lookahead.new(
                      assertion => RakuAST::Regex::Assertion::Named::RegexArg.new(
                        name      => RakuAST::Name.from-identifier("before"),
                        regex-arg => RakuAST::Regex::Sequence.new(
                          RakuAST::Regex::Literal.new("bar")
                        )
                      )
                    )
                  )
                )
              )
            ),
            RakuAST::Regex::Literal.new("bar")
          )
        )
      )
    )
    END

is Q[/a $x b/].AST.gist, q:to/END/.chomp, 'a scalar interpolation remains an Interpolation node';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Sequence.new(
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Literal.new("a")
            ),
            RakuAST::Regex::WithWhitespace.new(
              RakuAST::Regex::Interpolation.new(
                sequential => False,
                var        => RakuAST::Var::Lexical.new("\$x")
              )
            ),
            RakuAST::Regex::Literal.new("b")
          )
        )
      )
    )
    END

is Q[grammar GRegexToken { token x { \d+ } }].AST.gist, q:to/END/.chomp, 'grammar token declarations use TokenDeclaration and WithWhitespace';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Grammar.new(
          name => RakuAST::Name.from-identifier("GRegexToken"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::TokenDeclaration.new(
                    name => RakuAST::Name.from-identifier("x"),
                    body => RakuAST::Regex::WithWhitespace.new(
                      RakuAST::Regex::QuantifiedAtom.new(
                        atom       => RakuAST::Regex::CharClass::Digit.new,
                        quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[grammar GRegexRule { rule x { "a" } }].AST.gist, q:to/END/.chomp, 'grammar rule declarations preserve a quoted regex atom';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Grammar.new(
          name => RakuAST::Name.from-identifier("GRegexRule"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::RuleDeclaration.new(
                    name => RakuAST::Name.from-identifier("x"),
                    body => RakuAST::Regex::WithWhitespace.new(
                      RakuAST::Regex::Quote.new(
                        RakuAST::QuotedString.new(
                          segments   => (
                            RakuAST::StrLiteral.new("a"),
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[grammar GRegexAdjacent { rule x { a[bc]d } }].AST.gist, q:to/END/.chomp, 'declaration regex trees retain adjacency around groups';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Grammar.new(
          name => RakuAST::Name.from-identifier("GRegexAdjacent"),
          body => RakuAST::Block.new(
            body => RakuAST::Blockoid.new(
              RakuAST::StatementList.new(
                RakuAST::Statement::Expression.new(
                  expression => RakuAST::RuleDeclaration.new(
                    name => RakuAST::Name.from-identifier("x"),
                    body => RakuAST::Regex::Sequence.new(
                      RakuAST::Regex::Literal.new("a"),
                      RakuAST::Regex::Group.new(
                        RakuAST::Regex::Sequence.new(
                          RakuAST::Regex::Literal.new("bc")
                        )
                      ),
                      RakuAST::Regex::WithWhitespace.new(
                        RakuAST::Regex::Literal.new("d")
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    END

is Q[regex x { a }].AST.gist, q:to/END/.chomp, 'a regex declaration is distinct from token';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::RegexDeclaration.new(
          name => RakuAST::Name.from-identifier("x"),
          body => RakuAST::Regex::WithWhitespace.new(
            RakuAST::Regex::Literal.new("a")
          )
        )
      )
    )
    END

is EVAL(Q[/a/].AST).raku, '/a/', 'a regex AST lowers through the existing execution path';
ok 'a' ~~ EVAL(Q[/a/].AST), 'a lowered regex still matches';
is EVAL(Q[grammar GRegexEval { token x { \d+ } }].AST).^name, 'GRegexEval',
    'a grammar regex AST lowers to the grammar type';

my $literal = RakuAST::Regex::Literal.new("a");
is $literal.text, 'a', 'regex literal text is available through its accessor';
ok $literal ~~ RakuAST::Regex::Atom, 'regex atoms retain their semantic type';
ok $literal ~~ RakuAST::Regex, 'regex nodes retain their Regex type';
my $beginning-anchor = RakuAST::Regex::Anchor::BeginningOfString.new;
ok $beginning-anchor ~~ RakuAST::Regex::Atom,
    'anchors retain their regex atom type';
is $beginning-anchor.^name, 'RakuAST::Regex::Anchor::BeginningOfString',
    'anchors expose their concrete model class';
my $interpolation = RakuAST::Regex::Interpolation.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[$x]),
);
is $interpolation.var.name, '$x', 'interpolation exposes its lexical variable';
ok $interpolation ~~ RakuAST::Regex::Atom, 'interpolation retains its regex atom type';
is $interpolation.sequential, False, 'scalar interpolation is non-sequential';
my $constructed = RakuAST::QuotedRegex.new(
    match-immediately => True,
    body              => RakuAST::Regex::Literal.new("test"),
    adverbs           => (RakuAST::ColonPair::True.new("i"),)
);
$_ = 'TEST';
is EVAL($constructed).raku, 'Match.new(:orig("TEST"), :from(0), :pos(4))',
    'a constructed match-immediate regex uses the existing matcher';

my $lookaround-arg = RakuAST::Regex::Assertion::Named::RegexArg.new(
    name => RakuAST::Name.from-identifier('before'),
    regex-arg => RakuAST::Regex::Literal.new('bar'),
);
my $lookahead = RakuAST::Regex::Assertion::Lookahead.new(
    assertion => $lookaround-arg,
);
ok $lookahead ~~ RakuAST::Regex::Assertion,
    'lookaround nodes retain their abstract assertion type';
is $lookahead.negated, False,
    'positive lookaround assertions default to non-negated';
is $lookahead.assertion.name.raku, 'RakuAST::Name.from-identifier("before")',
    'lookaround assertions expose their named assertion';
is $lookahead.assertion.regex-arg.text, 'bar',
    'named regex arguments expose their regex body';
my $bare-before-node = Q[/<before bar>/].AST.statements[0].expression.body;
is $bare-before-node.capturing, True,
    'an unprefixed lookaround retains its capturing policy';
my $dot-before-node = Q[/<.before bar>/].AST.statements[0].expression.body;
is $dot-before-node.capturing, False,
    'a dot-prefixed lookaround suppresses its named capture';
my $bare-after-node = Q[/<after foo>/].AST.statements[0].expression.body;
is $bare-after-node.capturing, True,
    'an unprefixed lookbehind retains its capturing policy';
my $dot-after-node = Q[/<.after foo>/].AST.statements[0].expression.body;
is $dot-after-node.capturing, False,
    'a dot-prefixed lookbehind suppresses its named capture';

my $positive-before = EVAL(Q[/foo <?before bar>/].AST);
ok 'foobar' ~~ $positive-before,
    'a lowered positive lookahead matches without consuming its assertion';
nok 'foobaz' ~~ $positive-before,
    'a lowered positive lookahead rejects a missing suffix';
my $negative-before = EVAL(Q[/foo <!before bar>/].AST);
nok 'foobar' ~~ $negative-before,
    'a lowered negative lookahead rejects its asserted suffix';
ok 'foobaz' ~~ $negative-before,
    'a lowered negative lookahead accepts a different suffix';
my $positive-after = EVAL(Q[/<?after foo> bar/].AST);
ok 'foobar' ~~ $positive-after,
    'a lowered positive lookbehind matches its preceding text';
nok 'bazbar' ~~ $positive-after,
    'a lowered positive lookbehind rejects different preceding text';
my $negative-after = EVAL(Q[/<!after foo> bar/].AST);
nok 'foobar' ~~ $negative-after,
    'a lowered negative lookbehind rejects its preceding text';
ok 'bazbar' ~~ $negative-after,
    'a lowered negative lookbehind accepts different preceding text';

is Q[/<alias=foo>/].AST.gist, q:to/END/.chomp, 'a subrule alias retains its assertion child';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Assertion::Alias.new(
            name      => "alias",
            assertion => RakuAST::Regex::Assertion::Named.new(
              name      => RakuAST::Name.from-identifier("foo"),
              capturing => True
            )
          )
        )
      )
    )
    END

is Q[/<foo>/].AST.gist, q:to/END/.chomp, 'a capturing subrule retains its Named assertion';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Assertion::Named.new(
            name      => RakuAST::Name.from-identifier("foo"),
            capturing => True
          )
        )
      )
    )
    END

is Q[/<.foo>/].AST.gist, q:to/END/.chomp, 'a dot-suppressed subrule omits capturing';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::Assertion::Named.new(
            name => RakuAST::Name.from-identifier("foo")
          )
        )
      )
    )
    END

my $named-subrule = RakuAST::Regex::Assertion::Named.new(
    name => RakuAST::Name.from-identifier('part'),
    capturing => True,
);
ok $named-subrule ~~ RakuAST::Regex::Assertion,
    'named subrule assertions retain their abstract assertion type';
is $named-subrule.name.raku, 'RakuAST::Name.from-identifier("part")',
    'named subrule assertions expose their Name child';
is $named-subrule.capturing, True,
    'named subrule assertions expose their capturing flag';

my $silent-subrule = RakuAST::Regex::Assertion::Named.new(
    name => RakuAST::Name.from-identifier('suffix'),
);
is $silent-subrule.capturing, False,
    'named subrule assertions default to non-capturing';

my $alias-subrule = RakuAST::Regex::Assertion::Alias.new(
    name => 'word',
    assertion => $named-subrule,
);
ok $alias-subrule ~~ RakuAST::Regex::Assertion,
    'subrule aliases retain their abstract assertion type';
is $alias-subrule.name, 'word', 'subrule aliases expose their alias name';
is $alias-subrule.assertion.name.raku, 'RakuAST::Name.from-identifier("part")',
    'subrule aliases expose the named assertion child';

my $subrule-grammar = EVAL(Q[grammar GRegexSubruleAlias {
    token TOP { <word=part> };
    token part { "a" };
}].AST);
is $subrule-grammar.^name, 'GRegexSubruleAlias',
    'a grammar with a subrule alias lowers through the existing pipeline';
my $subrule-match = $subrule-grammar.parse('a');
ok $subrule-match, 'a lowered subrule alias matches its target token';
is ~$subrule-match<word>, 'a', 'a subrule alias captures under its alias name';
is ~$subrule-match<part>, 'a', 'a subrule alias retains the original capture name';
is $subrule-match.hash.keys.sort.join(','), 'part,word',
    'a subrule alias retains both alias and original captures';

my $ordinary-subrule-grammar = EVAL(Q[grammar GRegexBareSubrule {
    token TOP { <part><.suffix> };
    token part { "a" };
    token suffix { "b" };
}].AST);
my $ordinary-subrule-match = $ordinary-subrule-grammar.parse('ab');
ok $ordinary-subrule-match,
    'a bare and dot-suppressed subrule lower through the existing matcher';
is ~$ordinary-subrule-match<part>, 'a',
    'a bare subrule retains its named capture';
ok !$ordinary-subrule-match<suffix>.defined,
    'a dot-suppressed subrule does not publish a named capture';

my $builtin-override-grammar = EVAL(Q[grammar GRegexBuiltinOverride {
    token TOP { <same> };
    token same { "a" };
}].AST);
ok $builtin-override-grammar.parse('a'),
    'a grammar-local subrule keeps precedence over a special builtin assertion';

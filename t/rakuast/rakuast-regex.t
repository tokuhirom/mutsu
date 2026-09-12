use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST regex tree support (ADR-0088, issue #8033). The parser keeps a
# source-level static regex tree and the converter maps it to the same node
# shapes Rakudo exposes. These examples are intentionally static: dynamic
# assertions and interpolations remain explicit follow-up boundaries.

plan 16;

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
my $constructed = RakuAST::QuotedRegex.new(
    match-immediately => True,
    body              => RakuAST::Regex::Literal.new("test"),
    adverbs           => (RakuAST::ColonPair::True.new("i"),)
);
$_ = 'TEST';
is EVAL($constructed).raku, 'Match.new(:orig("TEST"), :from(0), :pos(4))',
    'a constructed match-immediate regex uses the existing matcher';

use experimental :rakuast;
use Test;

# #18967: a spaced `** :? N` converts to RakuAST, as on rakudo 2026.11, and
# its RakuAST round trip matches like the source.

plan 3;

is Q[/ a ** :? 3 /].AST.gist, q:to/AST/.chomp, '.AST of a spaced colon-frugal range';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      body => RakuAST::Regex::WithWhitespace.new(
        RakuAST::Regex::QuantifiedAtom.new(
          atom       => RakuAST::Regex::WithWhitespace.new(
            RakuAST::Regex::Literal.new("a")
          ),
          quantifier => RakuAST::Regex::Quantifier::Range.new(
            min       => 3,
            max       => 3,
            backtrack => RakuAST::Regex::Backtrack::Frugal
          )
        )
      )
    )
  )
)
AST

is ~("aaaa" ~~ Q[/ a ** :? 2..3 /].AST.EVAL), 'aa', 'the converted regex is frugal';
is ~("aaaa" ~~ Q[/ a ** :! 2..3 /].AST.EVAL), 'aaa', 'the converted regex is greedy';

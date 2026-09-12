use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: positional capture groups are retained in the shared
# source tree and lower to the existing capture-aware RegexPattern matcher.

plan 14;

is Q[/(a)/].AST.gist, q:to/END/.chomp, 'the parser exposes a capturing group instead of an opaque regex value';
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

my $parsed = / (a) /;
my $parsed-match = 'a' ~~ $parsed;
ok $parsed-match, 'a parser-produced capture group matches';
is $parsed-match[0].Str, 'a', 'a parser-produced capture stores its text';
is $parsed-match[0].from, 0, 'a parser-produced capture stores its start';
is $parsed-match[0].to, 1, 'a parser-produced capture stores its end';

my $constructed-group = RakuAST::Regex::CapturingGroup.new(
    RakuAST::Regex::Literal.new("a"),
);
is $constructed-group.regex.text, 'a', 'a capturing group exposes its regex accessor';
ok $constructed-group ~~ RakuAST::Regex::Atom,
    'a capturing group retains the regex atom semantic type';

my $constructed = RakuAST::QuotedRegex.new(
    body => $constructed-group,
);
my $constructed-value = EVAL($constructed);
my $constructed-match = 'a' ~~ $constructed-value;
ok $constructed-match, 'a constructed capture group lowers through EVAL';
is $constructed-match[0].Str, 'a', 'the lowered capture keeps its text';
is $constructed-match[0].from, 0, 'the lowered capture keeps its start';
is $constructed-match[0].to, 1, 'the lowered capture keeps its end';

my $quantified = 'aaa' ~~ /(a)+/;
is $quantified[0].elems, 3, 'a quantified capture retains one slot per iteration';
is $quantified[0][2].Str, 'a', 'the final quantified capture retains its value';
is ('aaa' ~~ /(a)+/)[0].elems, 3, 'a captured tree can be lowered again';

use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: ordinary scalar named captures retain their source
# node and lower to the existing named-capture matcher. Subrule aliases,
# array/hash aliases, and code-bearing regex nodes remain separate boundaries.

plan 16;

is Q[/ $<word> = a /].AST.gist, q:to/END/.chomp, 'a named capture keeps its whitespace and source node';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::WithWhitespace.new(
            RakuAST::Regex::NamedCapture.new(
              name  => "word",
              regex => RakuAST::Regex::Literal.new("a")
            )
          )
        )
      )
    )
    END

is Q[/$<word>=a+/].AST.gist, q:to/END/.chomp, 'a named capture can contain a quantified atom';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::QuotedRegex.new(
          body => RakuAST::Regex::NamedCapture.new(
            name  => "word",
            regex => RakuAST::Regex::QuantifiedAtom.new(
              atom       => RakuAST::Regex::Literal.new("a"),
              quantifier => RakuAST::Regex::Quantifier::OneOrMore.new
            )
          )
        )
      )
    )
    END

my $single = 'a' ~~ /$<word>=a/;
ok $single, 'a parser-created named capture matches';
is $single<word>.Str, 'a', 'the named capture stores its text';
is $single<word>.from, 0, 'the named capture stores its start';
is $single<word>.to, 1, 'the named capture stores its end';

my $quantified = 'aaa' ~~ /$<word>=a+/;
ok $quantified, 'a quantified named capture matches';
is $quantified<word>.Str, 'aaa', 'a scalar alias captures the whole run';
is $quantified<word>.from, 0, 'the quantified alias starts at the atom';
is $quantified<word>.to, 3, 'the quantified alias ends after the run';

my $node = RakuAST::Regex::NamedCapture.new(
    name  => 'word',
    regex => RakuAST::Regex::Literal.new('a'),
);
is $node.name, 'word', 'NamedCapture exposes its name';
nok $node.array, 'a scalar NamedCapture defaults to array False';
ok $node ~~ RakuAST::Regex::Atom, 'NamedCapture retains its regex atom type';
is $node.regex.text, 'a', 'NamedCapture exposes its regex child';

my $constructed = EVAL(RakuAST::QuotedRegex.new(body => $node));
my $constructed-match = 'a' ~~ $constructed;
ok $constructed-match, 'a constructed named capture lowers through EVAL';
is $constructed-match<word>.Str, 'a', 'the lowered named capture keeps its text';

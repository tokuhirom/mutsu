use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: an array-sigil named capture keeps its array context
# in the shared RegexTree and lowers through the existing capture matcher.
# Hash aliases, subrules, and code-bearing regex nodes remain separate
# boundaries.

plan 19;

my $ast = Q[/@<word>=(a)/].AST;
my $capture = $ast.statements[0].expression.body;
is $capture.^name, 'RakuAST::Regex::NamedCapture',
    'an array-sigil alias retains the NamedCapture node';
is $capture.name, 'word', 'the array alias retains its name';
ok $capture.array, 'the array alias retains array context';
is $capture.regex.regex.text, 'a', 'the array alias retains its regex child';
is $capture.gist, q:to/END/.chomp, 'array context does not change the node shape';
    RakuAST::Regex::NamedCapture.new(
      name  => "word",
      regex => RakuAST::Regex::CapturingGroup.new(
        RakuAST::Regex::Literal.new("a")
      )
    )
    END

my $plain = 'a' ~~ /@<plain>=a/;
ok $plain, 'an array alias around a plain atom matches';
is $plain<plain>.WHAT, Match,
    'an array alias around a plain atom remains a single Match';
is $plain<plain>.Str, 'a', 'the plain array alias stores its text';

my $single = 'a' ~~ /@<word>=(a)/;
ok $single, 'a parser-created array alias matches';
is $single<word>.WHAT, Array, 'a non-quantified array alias returns an Array';
is $single<word>.elems, 1, 'a non-quantified array alias has one entry';
is $single<word>[0].Str, 'a', 'the array alias entry stores its text';

my $quantified = 'aaa' ~~ /@<word>=(a)+/;
ok $quantified, 'a quantified array alias matches';
is $quantified<word>.WHAT, Array, 'a quantified array alias returns an Array';
is $quantified<word>.elems, 3, 'a quantified array alias stores each iteration';
is $quantified<word>[2].Str, 'a', 'the final array alias entry stores its text';

my $node = RakuAST::Regex::NamedCapture.new(
    name  => 'word',
    array => True,
    regex => RakuAST::Regex::CapturingGroup.new(
        RakuAST::Regex::Literal.new('a'),
    ),
);
my $constructed = EVAL(RakuAST::QuotedRegex.new(body => $node));
my $constructed-match = 'a' ~~ $constructed;
ok $constructed-match, 'a constructed array alias lowers through EVAL';
is $constructed-match<word>.WHAT, Array,
    'constructed array alias keeps array context through EVAL';
is $constructed-match<word>[0].Str, 'a',
    'constructed array alias keeps the captured text';

use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-10723 S10: `$name=ATOM` in a regex (an alias to a variable). Rakudo
# 2026.09 renders only `Regex::Interpolation($name)` and drops the aliased
# atom; mutsu renders the same public node and keeps the atom in a hidden
# field, so the round trip matches what the written regex matches.

plan 13;

is Q[my $foo; m/a  $foo=[<two>]  d/].AST.gist, q:to/END/.chomp, 'the alias renders as rakudo renders it';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("foo")
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      match-immediately => True,
      body              => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::WithWhitespace.new(
          RakuAST::Regex::Literal.new("a")
        ),
        RakuAST::Regex::Interpolation.new(
          sequential => False,
          var        => RakuAST::Var::Lexical.new("\$foo")
        ),
        RakuAST::Regex::Literal.new("d")
      )
    )
  )
)
END

is Q[my $foo; m/$foo=(..)+/].AST.gist, q:to/END/.chomp, 'a quantified alias is still the bare interpolation';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::VarDeclaration::Simple.new(
      sigil       => "\$",
      desigilname => RakuAST::Name.from-identifier("foo")
    )
  ),
  RakuAST::Statement::Expression.new(
    expression => RakuAST::QuotedRegex.new(
      match-immediately => True,
      body              => RakuAST::Regex::Interpolation.new(
        sequential => False,
        var        => RakuAST::Var::Lexical.new("\$foo")
      )
    )
  )
)
END

my $interpolation = Q[my $foo; m/$foo=x/].AST.statements[1].expression.body;
isa-ok $interpolation, RakuAST::Regex::Interpolation, 'the read node is an interpolation';
is $interpolation.var.name, '$foo', 'its variable is the aliased one';
nok $interpolation.raku.contains('source'), 'the aliased atom is not part of the constructor form';
my @children;
$interpolation.visit-children({ @children.push: .^name });
is-deeply @children, ['RakuAST::Var::Lexical'], 'visit-children visits only the variable';

my $foo = 'bc';
my regex two { .. }
for Q["abc=bcd" ~~ m/a  $foo=[<two>]  d/],
    Q["abc=bc" ~~ m/a $foo=(..)+/],
    Q["abc=bc d" ~~ m:s/a $foo=[<two>] d/],
    Q["abcd" ~~ m/a $foo d/] -> $source {
    is (EVAL($source.AST) // 'Nil').Str, (EVAL($source) // 'Nil').Str,
        "the round trip matches what the source matches: $source";
}
is $foo, 'bc', 'the round trip leaves the variable as the source does';

my $built = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('a'),
        RakuAST::Regex::Interpolation.new(
            sequential => False,
            var        => RakuAST::Var::Lexical.new('$foo'),
        ),
        RakuAST::Regex::Literal.new('d'),
    ),
);
my $rx = EVAL $built;
ok 'abcd' ~~ $rx, 'a hand-built interpolation stays a plain interpolation';
nok 'abc=d' ~~ $rx, 'a hand-built interpolation has no aliased atom';

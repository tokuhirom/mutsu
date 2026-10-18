use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 8;

is Q[my token x is export { a }].AST.gist, q:to/END/.chomp, 'bare export is retained on a regex declaration';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::TokenDeclaration.new(
      scope  => "my",
      name   => RakuAST::Name.from-identifier("x"),
      traits => (
        RakuAST::Trait::Is.new(
          name => RakuAST::Name.from-identifier("export")
        ),
      ),
      body   => RakuAST::Regex::WithWhitespace.new(
        RakuAST::Regex::Literal.new("a")
      )
    )
  )
)
END

my $bare = Q[my token x is export { a }].AST;
lives-ok { EVAL($bare) }, 'a source declaration with bare export lowers';

my $tagged = Q[my rule x is export(:tag) { a }].AST;
ok $tagged.gist.contains('ColonPair::True.new("tag")'),
    'tagged export keeps its tag argument in the trait';
lives-ok { EVAL($tagged) }, 'a source declaration with tagged export lowers';

ok Q[our token x is export(:tag) { a }].AST.gist.contains('scope  => "our"'),
    'package-scoped declarations retain their scope';
ok !Q[token x is export { a }].AST.gist.contains('scope  =>'),
    'unscoped declarations omit the default scope';

my $hand-built = RakuAST::TokenDeclaration.new(
    name => RakuAST::Name.from-identifier("x"),
    traits => (RakuAST::Trait::Is.new(
        name => RakuAST::Name.from-identifier("export")
    ),),
    body => RakuAST::Regex::Literal.new("a")
);
lives-ok { EVAL($hand-built) }, 'a hand-built export trait lowers';

ok Q[my regex x is export(:tag) { a }].AST.gist.contains('Trait::Is.new('),
    'regex declaration export uses a Trait::Is name';

use Test;

plan 5;

# `.raku` of a type over a plain name is the `from-identifier` call that builds
# it, not `.new` around a `Name` (identical under rakudo).
is RakuAST::Type::Simple.new(RakuAST::Name.from-identifier("S")).raku,
    'RakuAST::Type::Simple.from-identifier("S")', 'Type::Simple, one part';
is RakuAST::Type::Simple.new(RakuAST::Name.from-identifier-parts("A", "B")).raku,
    'RakuAST::Type::Simple.from-identifier-parts("A","B")', 'Type::Simple, several parts';
is RakuAST::Type::Setting.new(RakuAST::Name.from-identifier("Int")).raku,
    'RakuAST::Type::Setting.from-identifier("Int")', 'Type::Setting';
is RakuAST::Type::Capture.new(RakuAST::Name.from-identifier("T")).raku,
    'RakuAST::Type::Capture.from-identifier("T")', 'Type::Capture';

my $decl = Q|my Int $x|.AST.raku;
like $decl, / 'type        => RakuAST::Type::Simple.from-identifier("Int"),' /, 'inside a declaration';

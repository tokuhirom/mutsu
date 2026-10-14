use Test;

# `RakuAST::Trait::Will.new(:phase, :block)`, `RakuAST::Trait::Trusts.new(:type)`
# and `RakuAST::Statement::Trusts.new(:type)` construct by hand (#13133),
# measured on rakudo 2026.09. A class's `Trait::Trusts` is the trait form of a
# `trusts B;` statement: rakudo deparses it as one in the body, and EVAL of the
# class records B in `.^trusts`.

plan 13;

my $type = RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('TrustedOne'));

# --- Trait::Trusts
my $trusts = RakuAST::Trait::Trusts.new(type => $type);
isa-ok $trusts, RakuAST::Trait::Trusts, 'Trait::Trusts.new(type => ...) constructs';
isa-ok $trusts, RakuAST::Trait, '... and is a RakuAST::Trait';
is $trusts.type.name.canonicalize, 'TrustedOne', '... with the type accessor';
is $trusts.raku, 'RakuAST::Trait::Trusts.new(
  type => RakuAST::Type::Simple.new(
    RakuAST::Name.from-identifier("TrustedOne")
  )
)', '... and renders as rakudo does';

class TrustedOne { }
my $class = RakuAST::Class.new(
    name => RakuAST::Name.from-identifier('TrustingOne'),
    traits => ($trusts,));
my $evaled = $class.EVAL;
is $evaled.^name, 'TrustingOne', 'EVAL of a class carrying a hand-built Trait::Trusts';
is $evaled.^trusts.map(*.^name).join(','), 'TrustedOne', '... trusts the named type';

# --- Statement::Trusts (the parsed form)
my $stmt = RakuAST::Statement::Trusts.new(type => $type);
isa-ok $stmt, RakuAST::Statement::Trusts, 'Statement::Trusts.new(type => ...) constructs';
my $parsed-class = Q[class TrustingTwo { trusts TrustedOne }].AST.statements[0].expression;
my $parsed = $parsed-class.body.body.statement-list.statements[0];
is $parsed.raku, $stmt.raku, '... equal to the parsed `trusts` statement';

# --- Trait::Will
my $block = RakuAST::Block.new;
my $will = RakuAST::Trait::Will.new(phase => 'leave', block => $block);
isa-ok $will, RakuAST::Trait::Will, 'Trait::Will.new(:phase, :block) constructs';
is $will.phase, 'leave', '... with the phase accessor';
isa-ok $will.block, RakuAST::Block, '... and the block accessor';

my $decl = Q[my $x will begin { $_ = 7 }; $x].AST;
my $hand = $decl.statements[0].expression.traits[0];
my $built = RakuAST::Trait::Will.new(phase => $hand.phase, block => $hand.block);
is $built.raku, $hand.raku, 'a hand-built Trait::Will equals the parsed one';

# A hand-built declaration carrying it runs the block in its phase.
my $hand-decl = RakuAST::VarDeclaration::Simple.new(
    sigil => '$', desigilname => RakuAST::Name.from-identifier('x'), traits => ($built,));
is RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $hand-decl),
    $decl.statements[1]).EVAL, 7, 'EVAL of a declaration with a hand-built Trait::Will';

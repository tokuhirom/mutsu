use v6;
use experimental :rakuast;
use Test;

# `is export` on a `constant` or an `our` variable is a `Trait::Is` of the
# declaration in rakudo's tree (measured on 2026.09); the parser keeps it as
# the declaration's export flag and tags, which the round trip must not drop.

plan 8;

my $constant = Q[constant k is export = 8].AST.statements[0].expression;
isa-ok $constant, RakuAST::VarDeclaration::Constant, 'an exported constant';
is $constant.traits.map(*.raku).join, 'RakuAST::Trait::Is.new(' ~ "\n"
    ~ '  name => RakuAST::Name.from-identifier("export")' ~ "\n" ~ ')',
    'its bare `is export` is a Trait::Is';

my $tagged = Q[our @v is export(:t) = 1, 2].AST.statements[0].expression;
isa-ok $tagged, RakuAST::VarDeclaration::Simple, 'an exported our variable';
is $tagged.traits.elems, 1, 'it has one trait';
like $tagged.traits[0].raku, /'ColonPair::True.new("t")'/,
    'the export tag is the trait argument';

is Q[constant k = 8].AST.statements[0].expression.traits.elems, 0,
    'a constant that is not exported has no traits';

# The trait is what exports the declaration: a constant nested in a routine
# body is exported at compile time although the routine never runs.
is EVAL(Q[
    module M { sub f { constant k is export = 8 } }
    import M;
    k
].AST), 8, 'a nested exported constant round-trips';

is EVAL(Q[
    module N { our $v is export(:t) = 3 }
    import N :t;
    $v
].AST), 3, 'a tagged exported our variable round-trips';

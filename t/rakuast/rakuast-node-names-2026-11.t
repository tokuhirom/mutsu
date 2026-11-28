use Test;

# The RakuAST type names of Rakudo 2026.11's setting (#19274): the new node
# classes resolve, RakuAST::Expression::QAST (removed in 2026.11) does not,
# and the classes under RakuAST::IMPL (whose WHO is now a raw NQP hash) still
# do, as on rakudo.

plan 10;

for RakuAST::MetaInfix::CrossZip, RakuAST::Name::Part::EmptyEdge,
    RakuAST::StatementModifier::IfUnless,
    RakuAST::VarDeclaration::Implicit::HeaderValue, RakuAST::WhereThunk -> \T {
    nok T.DEFINITE, "{T.^name} resolves";
}
is RakuAST::Var::PackageHeaderLookup.^name, 'RakuAST::Var::PackageHeaderLookup',
    'the PackageHeaderLookup role resolves';
is RakuAST::IMPL::QASTContext.^name, 'RakuAST::IMPL::QASTContext',
    'RakuAST::IMPL::QASTContext still resolves';
is RakuAST::DottyInfix::Call.^name, 'RakuAST::DottyInfix::Call',
    'a class under a raw-hash WHO resolves';

throws-like 'RakuAST::Expression::QAST.^name', Exception,
    message => /"Could not find symbol '&QAST'"/, 'RakuAST::Expression::QAST is gone';
ok ::('RakuAST::Expression::QAST') ~~ Failure, 'and does not resolve indirectly';

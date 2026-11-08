use Test;
use nqp;

# `$*CU.resolver` / `.context` and the `IMPL-BEGIN` / `IMPL-CHECK` hooks a
# CHECK-phase rewrite runs on a node it built.
plan 5;

my ($r, $ctx, $hooked);
BEGIN {
    $r = nqp::getattr($*CU, RakuAST::CompUnit, '$!resolver');
    $ctx = $*CU.context;
    my $n = RakuAST::Call::Name.new(name => RakuAST::Name.from-identifier('say'),
        args => RakuAST::ArgList.new(RakuAST::StrLiteral.new("x")));
    $n.IMPL-BEGIN($r, $ctx);
    $n.IMPL-CHECK($r, $ctx);
    $hooked = True;
}
is $r.^name, 'RakuAST::Resolver::Compile', 'the unit resolver';
is $ctx.^name, 'RakuAST::IMPL::QASTContext', 'the unit context';
ok $hooked, 'IMPL-BEGIN and IMPL-CHECK accept the resolver and context';
ok RakuAST::Node.can('IMPL-BEGIN'), 'Node can IMPL-BEGIN';
ok RakuAST::Node.can('IMPL-CHECK'), 'Node can IMPL-CHECK';

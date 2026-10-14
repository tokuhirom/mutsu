use Test;

plan 5;

my $ast = Q[given 5 { when 5 { succeed |() } }].AST;
ok $ast.raku.contains('RakuAST::Call::Name::WithoutParentheses.new('), 'succeed |() is a bare call';
ok $ast.raku.contains('name => RakuAST::Name.from-identifier("succeed")'), 'call name is succeed';
ok $ast.raku.contains('prefix  => RakuAST::Prefix.new("|")'), 'the argument is the slip prefix';

my @w;
EVAL Q[
given 5 {
    when 5 { @w.push('when'); succeed |(); @w.push('not reached') }
    default { @w.push('default') }
}
].AST;
is @w, ['when'], 'EVAL of the round-tripped succeed |() leaves the given';

my @p;
EVAL Q[
given 5 {
    when 5 { @p.push('when'); proceed |(); @p.push('not reached') }
    default { @p.push('default') }
}
].AST;
is @p, ['when', 'default'], 'EVAL of the round-tripped proceed |() falls to default';

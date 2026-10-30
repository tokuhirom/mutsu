use Test;

# A string-literal return constraint with escapes converts through `.AST`
# (`method reduce-triangle(--> '[\\')` in rakudo's own RakuAST::Deparse).
plan 5;

sub returns-of(Str $code) {
    $code.AST.statements[0].expression.signature.returns
}

for Q|sub f(--> '[\\') { }|, Q|sub f(--> "\{\n") { }|, Q|sub f(--> 'it\'s') { }| -> $code {
    my $ret = returns-of($code);
    isa-ok $ret, RakuAST::QuotedString, "a literal with escapes converts: $code";
}
is returns-of(Q|sub f(--> '[\\') { }|).segments[0].value, '[\\', 'the single-quoted escape is decoded';
is returns-of(Q|sub f(--> "\{\n") { }|).segments[0].value, "\{\n", 'the double-quoted escapes are decoded';

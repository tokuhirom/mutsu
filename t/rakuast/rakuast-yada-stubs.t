use Test;

# The yada-yada stubs in RakuAST, measured on rakudo 2026.09: `...` is a
# `Stub::Fail`, `!!!` a `Stub::Die`, `???` a `Stub::Warn`, each with an
# `args` list only when the source wrote a message.

plan 9;

sub body-expr($src) { $src.AST.statements.head.expression.body.statement-list.statements.head.expression }

isa-ok body-expr(Q[sub f { ... }]), RakuAST::Stub::Fail, '`...` is a Stub::Fail';
isa-ok body-expr(Q[sub f { !!! }]), RakuAST::Stub::Die, '`!!!` is a Stub::Die';
isa-ok body-expr(Q[sub f { ??? }]), RakuAST::Stub::Warn, '`???` is a Stub::Warn';
is body-expr(Q[sub f { ... }]).args.args.elems, 0, 'a stub without a message has empty args';
isa-ok body-expr(Q[sub f { ... "msg" }]).args, RakuAST::ArgList, 'a message is its args';

is EVAL(Q[sub f { ... }; my $x = f(); $x.exception.message].AST),
    'Stub code executed', '`...` still fails after the round trip';
is EVAL(Q[sub f { ... "nope" }; f().exception.message].AST),
    'nope', 'and keeps its message';
is EVAL(Q[sub g { !!! }; my $alive = True; try { g(); $alive = False }; $alive].AST),
    True, '`!!!` still dies';
is EVAL(Q[(sub { ... }).yada].AST), True, 'a stub routine is still a stub';

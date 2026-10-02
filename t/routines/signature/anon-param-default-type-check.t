use Test;

plan 4;

# A wrong-typed argument for an anonymous parameter with a default is a
# run-time binding failure, not rakudo's compile-time "will never work".
sub h(Int $ = 5) { 2 }
is h(), 2, 'default is used';
is h(7), 2, 'matching argument binds';

try h("x");
isa-ok $!, X::TypeCheck::Binding::Parameter, 'wrong type fails the binding check';
like $!.message, /'Type check failed in binding to parameter' .* 'expected Int but got Str'/,
    'message is the run-time form';

done-testing;

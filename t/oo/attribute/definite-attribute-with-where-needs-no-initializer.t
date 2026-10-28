use Test;

plan 4;

# OO::Plugin (ecosystem): `has Str:D $.stage is rw where * ~~ any <before around after>;`
# compiles on rakudo -- a `where` clause makes the attribute a constrained
# subtype, and the `:D` initializer demand applies only to the bare form.
lives-ok { EVAL 'class WithWhere { has Str:D $.stage is rw where * ~~ any <before around after>; }' },
    'Str:D with where, no initializer';
lives-ok { EVAL 'class WithWhere2 { has Int:D $.n where * > 0; }' }, 'Int:D with where';

throws-like { EVAL 'class Bare { has Str:D $.s; }' }, X::Syntax::Variable::MissingInitializer,
    'the bare :D attribute still needs an initializer';

class Ok { has Int:D $.n where * > 0; }
is Ok.new(n => 3).n, 3, 'the constraint still applies at construction';

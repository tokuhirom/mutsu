use lib 't/lib';
use experimental :rakuast;
use Test;
use SupersedeBuiltinHOW;

# Rakudo's Class and Role trees keep this unit's effective HOW out of the
# rendered constructor form. The internal round-trip marker must stay hidden.

plan 8;

my $class = Q[class Foo { }].AST.statements.head.expression;
isa-ok $class, RakuAST::Class, 'a superseded class remains a Class node';
is $class.parsed-declarator, 'class', 'the built-in declarator remains class';
ok !$class.gist.contains('parsed-declarator'), 'the default declarator is not rendered';
ok !$class.gist.contains('source-supersede'), 'the source marker is not rendered';

my $role = Q[role Bar { }].AST.statements.head.expression;
isa-ok $role, RakuAST::Role, 'a superseded role remains a Role node';
is $role.parsed-declarator, 'role', 'the built-in declarator remains role';
ok !$role.gist.contains('parsed-declarator'), 'the default declarator is not rendered';
ok !$role.gist.contains('source-supersede'), 'the source marker is not rendered';

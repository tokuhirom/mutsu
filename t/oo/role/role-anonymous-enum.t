use v6;
use Test;

# Found via the LibXML distribution (LibXML::Raw::DOM::Node): an anonymous
# `enum <A B>` inside a role installs no package symbol, so rakudo accepts it,
# while a named enum is still rejected as our-scoped.

plan 6;

role R1 {
    enum <A B>;
    method a { A }
    method b { B }
}
class C does R1 { }

is C.new.a.value, 0, 'anonymous enum value inside a role (first)';
is C.new.b.value, 1, 'anonymous enum value inside a role (second)';
is C.new.b.key, 'B', 'anonymous enum key inside a role';

role R2 {
    my enum E2 <X Y>;
    method x { X }
}
class D does R2 { }
is D.new.x.value, 0, 'my enum inside a role is still allowed';

throws-like 'my role R { enum Tea <green black fruit> }',
    X::Declaration::OurScopeInRole, declaration => 'enum',
    'a named enum inside a role is still rejected';
throws-like 'my role R { our enum Tea <green black fruit> }',
    X::Declaration::OurScopeInRole, declaration => 'enum',
    'an explicit our enum inside a role is still rejected';

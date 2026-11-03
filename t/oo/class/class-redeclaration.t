use Test;

# Declaring the same class name twice in one lexical scope is a compile-time
# X::Redeclaration ("Redeclaration of symbol 'A'"), matching Raku. mutsu used to
# silently accept the second declaration. A stub (`class A {...}`) followed by
# its real definition is NOT a redeclaration, and a same-named class in an inner
# block shadows (rather than redeclares) an outer one.

plan 7;

throws-like 'class A {}; class A {}', X::Redeclaration,
    'same-scope class redeclaration throws X::Redeclaration';
throws-like 'class B { has $.x }; class B { has $.y }', X::Redeclaration,
    'redeclaration with different bodies still throws';
throws-like 'role R {}; class R {}', X::Redeclaration,
    'a class of the name of a role in the same scope throws X::Redeclaration';
# Roles of one name are candidates of one role group, as in rakudo.
lives-ok { EVAL 'role G {}; role G {}; 1' },
    'two roles of one name do not redeclare';

# A stub followed by its real definition is legal.
lives-ok { EVAL 'class Node { ... }; class Node { has $.v }; Node.new(v => 1)' },
    'stub class followed by its real definition is not a redeclaration';

# Same class name in two separate blocks shadows, not redeclares.
lives-ok { EVAL '{ class C { has $.x } }; { class C { has $.y } }' },
    'same-named class in separate blocks does not redeclare';

# A single ordinary declaration is of course fine.
lives-ok { EVAL 'class D { has $.z }; D.new(z => 2)' },
    'single class declaration is fine';

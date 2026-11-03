use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# EVAL of a tree (#15869): declarations that rakudo accepts at compile time
# are not redeclarations when the tree is evaluated.

plan 8;

# A `proto` in a nested block shadows the outer family instead of redeclaring it.
is EVAL(q:to/CODE/.AST), 'inner/outer', 'a nested-block proto+multi shadows the outer family';
    proto d2($) {*}
    multi d2(Int $x) { "outer" }
    my $r = do { proto d2($) {*}; multi d2(Int $x) { "inner" }; d2(1) };
    "$r/" ~ d2(1);
    CODE

# Forward declarations may repeat before the definition.
is EVAL(q:to/CODE/.AST), 26, 'a repeated stub is completed by one definition';
    sub stubbed {...}
    sub stubbed {...}
    sub stubbed {26}
    stubbed();
    CODE

# Roles of one name are the candidates of one role group.
is EVAL(q:to/CODE/.AST), 'small2', 'role group candidates are not redeclarations';
    role P[$n where * > 3] { method p { "big$n" } }
    role P[$n] { method p { "small$n" } }
    (1 but P[2]).p;
    CODE

# What is still a redeclaration stays one.
throws-like { EVAL(q[sub dup1() { 1 }; sub dup1() { 2 }].AST) }, X::Redeclaration,
    'two plain subs of one scope';
throws-like { EVAL(q[proto dup2($) {*}; proto dup2($) {*}].AST) }, X::Redeclaration,
    'two protos of one scope';
throws-like { EVAL(q[proto dup3($) {*}; sub dup3($x) { 1 }].AST) }, X::Redeclaration,
    'a proto and a plain sub of one scope';
throws-like { EVAL(q[role R4 { }; class R4 { }].AST) }, X::Redeclaration,
    'a class named like a role';
is EVAL(q[sub s5 { 'outer' }; { sub s5 { 'inner' }; s5() }].AST), 'inner',
    'a plain nested sub shadows';

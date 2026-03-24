use Test;

plan 14;

{
    my role R1[::T] { method x { T } }
    my class C1 does R1[Int] { }
    my R1 of Int $x = C1.new;
    isa-ok $x.x, Int, "variable declaration accepts role of type syntax";
}

{
    my role R1[::T] { method x { T } }
    my class C1 does R1[Int] { }
    sub value-type(R1 of Int $x) { $x.x }
    isa-ok value-type(C1.new), Int, "parameter accepts role of type syntax";
}

{
    my role R2[::T] { method x { "ok" } }
    my class C3 does R2[R2[Int]] { }
    my R2 of R2 of Int $x = C3.new;
    is $x.x, "ok", "nested of chains normalize to nested parameterized role types";
}

throws-like 'my role R1[::T] { }; my R1 of Str $x = R1[Int].new;',
    X::TypeCheck::Assignment,
    'variable assignment still enforces normalized of type constraint';

throws-like 'my role R1[::T] { method x { T } }; sub f(R1 of Int $x) { $x.x }; f(R1[Str].new)',
    X::TypeCheck::Binding,
    'parameter binding still enforces normalized of type constraint';

{
    my role R1[::T] { method x { T } }
    isa-ok R1[Int].new.x, Int, "direct role instantiation preserves parameterized role type metadata";
}

throws-like 'role ABCD[EFGH] { }',
    X::Parameter::InvalidType,
    'undefined bare type in role parameter list throws';

{
    my role R2[::T] {
        method call_test { self.call_test_helper(T.new) }
        method call_test_helper(T $x) { "ok" }
    }
    my class C3 does R2[R2[Int]] { }
    is C3.new.call_test, "ok", "composed class methods see substituted role parameter types";
}

{
    my role R2[::T] {
        method call_test_helper(T $x) { "ok" }
        method call_fail { self.call_test_helper(4.5) }
    }
    my class C3 does R2[R2[Int]] { }
    dies-ok { C3.new.call_fail },
        "composed class methods enforce substituted role parameter types";
}

throws-like q[role A[::T] { }; class C does A[::T] { }],
    Exception,
    'role application rejects type captures as arguments';

throws-like 'sub f(Int @x) {}; f( [] )',
    X::TypeCheck::Binding,
    message => /Positional\[Int\]/,
    'typed array binding reports Positional[Int]';

{
    my role TreeNode[::T] does Positional {
        has TreeNode[T] @!children handles <AT-POS ASSIGN-POS BIND-POS>;
        has T $.data is rw;
    }

    my $tree = TreeNode[Int].new;
    $tree.data = 3;
    $tree[0] = TreeNode[Int].new;
    $tree[1] = TreeNode[Int].new;
    $tree[0].data = 1;
    $tree[1].data = 4;

    is ($tree.data, $tree[0, 1]>>.data).flat.join(","), "3,1,4",
        "indexed writes on parametric Positional roles preserve other mixin attributes";
}

{
    my role R[::T] { multi method foo(T $t) { T.gist } }
    my class A does R[Str] does R[Int] { }
    is A.new.foo(5), 5.WHAT.gist, "lexical class with reused short name sets up suppression";
}

{
    my role A[::T] { method a { A[T] } }
    isa-ok A[Int].a, A[Int], "later role declaration clears stale short-name suppression";
}

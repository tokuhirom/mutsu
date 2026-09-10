use Test;

# A duplicate non-multi `sub` (or `token`/`regex`) declared inside a
# class / grammar / package / module body is an X::Redeclaration, just like at
# the top level. (Detected on the EVAL / throws-like compile path.)

throws-like q[class A { sub a { 1 }; sub a { 1 } }],
    X::Redeclaration, 'sub redeclared in class dies';
throws-like q[package P { sub p { 1 }; sub p { 1 } }],
    X::Redeclaration, 'sub redeclared in package dies';
throws-like q[module M { sub m { 1 }; sub m { 1 } }],
    X::Redeclaration, 'sub redeclared in module dies';
throws-like q[grammar G { token b { 'b' }; token b { 'b' } }],
    X::Redeclaration, 'token redeclared in grammar dies';

# A `multi sub` may be repeated; mixing multi and non-multi may not.
lives-ok { EVAL q[class OK { multi sub f($x) { 1 }; multi sub f($x, $y) { 2 } }] },
    'multi subs of the same name in a class are allowed';
throws-like q[class Bad { sub g { 1 }; multi sub g { 2 } }],
    X::Redeclaration, 'sub then multi sub of the same name dies';

# A single sub is fine.
lives-ok { EVAL q[class Fine { sub h { 1 } }] },
    'a single sub in a class is fine';

done-testing;

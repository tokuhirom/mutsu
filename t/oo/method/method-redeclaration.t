use Test;

plan 6;

# Two `my`/`our`-scoped methods of the same name in one class/role body are an
# X::Redeclaration (a plain `method foo` redeclared is the separate
# X::Method::Duplicate, not covered here).
throws-like 'my class C { my method foo { 1 }; my method foo { 2 } }',
    X::Redeclaration, 'duplicate my method';
throws-like 'my class C { our method foo { 1 }; our method foo { 2 } }',
    X::Redeclaration, 'duplicate our method';
throws-like 'my role R { my method foo { 1 }; my method foo { 2 } }',
    X::Redeclaration, 'duplicate my method in a role';

# Distinct method names — and a single method — are fine.
{
    lives-ok { EVAL 'my class C { my method a { 1 }; my method b { 2 } }' },
        'distinct my methods are allowed';
    lives-ok { EVAL 'my class C { our method a { 1 }; my method a { 2 } }' },
        'same name in different scopes is allowed';
}
{
    class D { method hi { 'hi' } }
    is D.new.hi, 'hi', 'an ordinary single method still works';
}

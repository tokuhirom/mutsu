use Test;

plan 6;

# `is Role` where the role inherits a class: the role's class-ancestor is
# linearized right after the role (C3-linearization order), not after siblings.
{
    my class C1 { }
    my class C2 is C1 { }
    my role R3a is C2 { }
    my role R3b { }
    my class C3 is R3a is R3b { }
    my class C4 is C3 { }
    is-deeply C4.^mro[1..5], (C3, R3a.^pun, C2, C1, R3b.^pun),
        'is-Role class-ancestor linearizes right after the role';
}

# `class C hides P` drops parent P from .^mro_unhidden (also via an ancestor).
{
    my class C1 { }
    my class C2 is C1 { }
    my class C3 hides C2 { }
    my class C4 is C3 { }
    is-deeply C4.^mro_unhidden[0..*-3], (C4, C3, C1), 'hides on a class (via ancestor)';
}

# `role R hides P; class C does R`: hide is preserved and the does-role is not
# an mro_unhidden entry.
{
    my class C1 { }
    my class C2 is C1 { }
    my role R3a hides C2 { }
    my class C3 does R3a { }
    my class C4 is C3 { }
    is-deeply C4.^mro_unhidden[0..*-3], (C4, C3, C1), 'hides on a consumed does-role';
}

# `is hidden` on a puned role.
{
    my class C1 { }
    my class C2 is C1 { }
    my role R3a is C2 is hidden { }
    my role R3b { }
    my class C3 is R3a is R3b { }
    my class C4 is C3 { }
    is-deeply C4.^mro_unhidden[0..*-3], (C4, C3, C2, C1, R3b.^pun), 'is hidden on a puned role';
}

# A plain does-role provides methods but is not an mro_unhidden entry.
{
    role Greet { method hi { 'hi' } }
    my class G does Greet { }
    ok G.new.hi eq 'hi', 'does-role method is composed';
    nok G.^mro_unhidden.map(*.^name).grep('Greet'), 'does-role is not an mro_unhidden entry';
}

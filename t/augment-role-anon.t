use Test;

plan 7;

# Roles are always closed: augmenting one is illegal.
throws-like 'use MONKEY-TYPING; augment role Positional { }',
    X::Syntax::Augment::Illegal;
throws-like 'use MONKEY-TYPING; augment role Associative { }',
    X::Syntax::Augment::Illegal;

# A user-defined role is also closed.
throws-like 'role R { }; use MONKEY-TYPING; augment role R { }',
    X::Syntax::Augment::Illegal;

# A non-existent role reports that first.
throws-like 'use MONKEY-TYPING; augment role NoSuchRole { }',
    X::Augment::NoSuchType, package-kind => 'role';

# An anonymous augment has no package name.
throws-like 'use MONKEY-TYPING; augment class { }',
    X::Anon::Augment, package-kind => 'class';
throws-like 'use MONKEY-TYPING; augment role { }',
    X::Anon::Augment, package-kind => 'role';

# Augmenting an existing class is still legal.
is-deeply EVAL('class C { method a { 1 } }; use MONKEY-TYPING; augment class C { method b { 2 } }; C.new.b'),
    2, 'augmenting an existing class still works';

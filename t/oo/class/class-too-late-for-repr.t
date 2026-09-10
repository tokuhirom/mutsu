use Test;

# A class's repr must be fixed at its initial declaration. Upgrading a `{ ... }`
# stub (or any earlier declaration) with a new `is repr(...)` is X::TooLateForREPR.

plan 4;

throws-like 'my class A { ... }; my class A is repr("Uninstantiable") { }',
    X::TooLateForREPR, 'adding a repr when upgrading a stub';

# Valid: repr set at the initial declaration.
lives-ok { EVAL 'my class B is repr("P6opaque") { }' },
    'repr at the initial declaration lives';

# Valid: a stub upgraded without introducing a repr.
lives-ok { EVAL 'my class C { ... }; my class C { method m { 1 } }' },
    'stub upgrade without a repr lives';

# Valid: the same repr repeated is not a change.
lives-ok { EVAL 'my class D is repr("P6opaque") { ... }; my class D is repr("P6opaque") { }' },
    'same repr on stub and upgrade lives';

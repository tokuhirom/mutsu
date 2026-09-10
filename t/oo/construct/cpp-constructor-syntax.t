use Test;

plan 8;

# `new TypeName` (C++ / indirect constructor syntax) is unsupported in Raku;
# it must be rejected with X::Obsolete. `TypeName.new` is the correct form.

throws-like 'class A { has $.b }; new A',
    X::Obsolete,
    'bare `new A` is rejected as C++ constructor syntax';

throws-like 'class A { has $.b }; new A( :b(1) )',
    X::Obsolete,
    '`new A(:b(1))` is rejected as C++ constructor syntax';

throws-like 'class A {}; my $x = new A.new',
    X::Obsolete,
    '`new A.new` is rejected as C++ constructor syntax';

throws-like 'new Int',
    X::Obsolete,
    '`new Int` on a built-in type is rejected';

# The correct method-call forms must still work.
{
    class C { has $.v }
    is C.new(v => 5).v, 5, '`C.new(...)` still works';
    is C.new.defined, True, '`C.new` still works';
}

# `new` as an ordinary sub name (lowercase args / parens) is unaffected.
{
    sub new($x) { $x * 2 }
    is new(21), 42, '`new(...)` as a sub call still works';
}

# `new` used as a hash pair key is unaffected.
{
    my %h = new => 7;
    is %h<new>, 7, '`new => ...` pair key still works';
}

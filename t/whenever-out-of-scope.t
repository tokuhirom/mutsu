use v6;
use Test;

plan 8;

# A `whenever` block outside the lexical scope of a `supply`/`react` block is a
# compile-time error: X::Comp::WheneverOutOfScope.

# whenever inside a sub, called from react — still out of scope (routine boundary).
throws-like ｢sub foo { whenever Promise.in(1) { say 1 } }; react foo｣,
    X::Comp::WheneverOutOfScope, 'whenever in a sub is out of react scope';

# whenever inside a raw pointy closure (not a supply block).
throws-like ｢my $c = -> $e { whenever Promise.in(1) { say 1 } }｣,
    X::Comp::WheneverOutOfScope, 'whenever in a raw closure is out of scope';

# bare top-level whenever.
throws-like ｢whenever Promise.in(1) { say 1 }｣,
    X::Comp::WheneverOutOfScope, 'bare top-level whenever is out of scope';

# whenever inside a method.
throws-like ｢class C { method m { whenever Promise.in(1) { say 1 } } }｣,
    X::Comp::WheneverOutOfScope, 'whenever in a method is out of scope';

# Legitimate uses must NOT throw.
lives-ok ｢react { whenever Promise.in(1) { done } }｣,
    'whenever directly in a react block is fine';

lives-ok ｢my $s = supply { whenever Promise.in(1) { emit 1 } }｣,
    'whenever directly in a supply block is fine';

lives-ok ｢react { for 1..2 { whenever Promise.in(1) { done } } }｣,
    'whenever in an inline for-loop inside react is fine';

lives-ok ｢react { whenever Promise.in(1) { whenever Promise.in(2) { done } } }｣,
    'nested whenever inside react is fine';

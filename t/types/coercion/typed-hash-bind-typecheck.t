use Test;

# Binding (`:=`) a hash to a typed-hash variable type-checks the container:
# the RHS must be an `Associative[T]` matching the declared value type.
# Assignment (`=`) coerces and is unaffected.

plan 11;

# An untyped/coerced hash bound to a typed hash dies.
dies-ok { my Int %h := :42foo.Set.Hash }, 'Set.Hash bound to Int %h dies';
dies-ok { my Int %h := { a => 1 } },       'hash literal bound to Int %h dies';
dies-ok { my %u = (a => 1); my Int %h := %u }, 'untyped hash bound to Int %h dies';
dies-ok { my Cool %a; my Int %b := %a },   'Cool hash bound to Int %h dies';

# A matching/conforming typed hash binds successfully (covariant).
lives-ok { my Int %a; my Int  %b := %a }, 'Int hash bound to Int %h lives';
lives-ok { my Int %a; my Cool %b := %a }, 'Int hash bound to Cool %h lives (covariant)';
lives-ok { my Int %a; my Any  %b := %a }, 'Int hash bound to Any %h lives';

# Untyped targets accept anything.
lives-ok { my %a; my %b := %a },          'untyped bound to untyped lives';
lives-ok { my %h := :42foo.Set.Hash },    'Set.Hash bound to untyped %h lives';

# The bound container keeps its own element type.
{
    my Int %a;
    my Cool %b := %a;
    is %b.of, Int, 'bound container keeps its declared element type';
}

# The exception is X::TypeCheck::Binding.
throws-like { my Int %h := { a => 1 } }, X::TypeCheck::Binding,
    'binding mismatch throws X::TypeCheck::Binding';

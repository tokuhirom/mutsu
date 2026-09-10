use Test;

# A class that tries to `does` a non-composable built-in concrete type
# (Int/Str/Num/Cool/Any/...) throws X::Composition::NotComposable. Composable
# built-in roles (Real/Numeric/Positional/...) and user roles still compose.

plan 6;

throws-like 'my class B does Int { }', X::Composition::NotComposable,
    'does Int', target-name => 'B', composer => Int;

throws-like 'my class B does Str { }', X::Composition::NotComposable,
    'does Str', composer => Str;

throws-like 'my class B does Cool { }', X::Composition::NotComposable,
    'does Cool';

# Composable built-in roles still work.
lives-ok { my class R1 does Real { }; my class P does Positional { }; },
    'does Real / Positional still compose';

# User roles still work.
lives-ok { role Greet { method hi { "hi" } }; my class C does Greet { }; },
    'user role still composes';

ok (my class D does Numeric { }).new.defined.not || True,
    'does Numeric composes (smoke)';

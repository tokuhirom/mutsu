use v6;
use Test;

# An `nqp::` op mutsu does not implement used to fall through the
# package-prefix strip in `call_function_fallback` and reach Raku's same-named
# builtin — with *different* semantics. `nqp::index("hello", "z")` returned
# Raku's Nil where nqp yields -1, and nqp code branches on exactly that
# (`!= -1`). A silent wrong answer is worse than an error, so an unimplemented
# nqp op now fails loudly and names itself.
#
# The `nqp::` namespace is reserved and its op set documented, so rejecting an
# unimplemented one is safe.

plan 7;

# The ops mutsu really does implement keep working (they are matched under
# their full `nqp::` name, before this guard).
{
    use nqp;
    is nqp::ordat("abc", 1), 98, 'an implemented nqp op still works';
}

throws-like 'use nqp; nqp::index("hello", "z")', X::AdHoc,
    message => /'Unsupported nqp:: op' .* 'nqp::index'/,
    'an unimplemented nqp op fails instead of aliasing to the Raku builtin';

throws-like 'use nqp; nqp::chars("x")', X::AdHoc,
    message => /'nqp::chars'/,
    'and names the op it could not provide';

throws-like 'use nqp; nqp::substr("hello", 1, 3)', X::AdHoc,
    message => /'nqp::substr'/,
    'including ops whose Raku namesake would have produced a plausible answer';

# Regression guard: this must stay scoped to `nqp::`. An ordinary qualified
# call still resolves through the package-prefix strip.
{
    module M { our sub f() { 42 } }
    is M::f(), 42, 'a qualified user sub still resolves';
}

# A non-`nqp` qualified call whose short name resolves to nothing fails too now
# — it used to reach Raku's `index` and return 2
# (news/2026-07/qualified-call-no-longer-aliases-a-builtin.md) — but it reports
# raku's own error, not this file's nqp-specific one, so the guard above stays
# scoped to `nqp::`.
throws-like 'Foo::Bar::index("hello", "l")', X::AdHoc,
    message => /"Could not find symbol '&index' in 'GLOBAL::Foo::Bar'"/,
    'a non-nqp qualified call reports raku\'s error, not the nqp one';

# `use nqp` itself stays a no-op pragma.
{
    lives-ok { EVAL 'use nqp; 1' }, '`use nqp` still loads';
}

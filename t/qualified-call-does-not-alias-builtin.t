use v6;
use Test;

# `call_function_fallback` ends with a package-prefix strip: an unresolved
# `Foo::bar(…)` retries as `bar(…)`, which is how a call qualified with a
# package mutsu never registered still finds its routine. Retrying
# unconditionally meant the qualifier was simply discarded and the call landed
# on Raku's same-named builtin — `Foo::Bar::index("hello", "l")` returned 2,
# and `Test::ok(1)` really ran a TAP assertion — where raku says
#
#     Could not find symbol '&index' in 'GLOBAL::Foo::Bar'
#
# The strip now runs only when mutsu has something *declared* under the short
# name. Every message below is the one rakudo produces for the same program.

plan 11;

throws-like 'Foo::Bar::index("hello", "l")', X::AdHoc,
    message => /"Could not find symbol '&index' in 'GLOBAL::Foo::Bar'"/,
    'a qualified call does not fall through to the same-named builtin';

throws-like 'Foo::Bar::say("x")', X::AdHoc,
    message => /"Could not find symbol '&say' in 'GLOBAL::Foo::Bar'"/,
    'including builtins that would otherwise have produced output';

# `Test` is loaded in this very file, and its routines are exported lexically —
# so a package-qualified call must not reach them either.
throws-like 'Test::ok(1)', X::AdHoc,
    message => /"Could not find symbol '&ok' in '" .* "Test'"/,
    'an exported routine is not reachable under its package name';

# A package mutsu *does* know is named bare in the error, as raku names it.
{
    module M { }
    throws-like 'M::nope()', X::AdHoc,
        message => /"Could not find symbol '&nope' in 'M'"/,
        'a known package is named without the GLOBAL:: prefix';
}

{
    class C { }
    throws-like 'C::foo()', X::AdHoc,
        message => /"Could not find symbol '&foo' in 'C'"/,
        'and so is a class';
}

# An explicitly written `GLOBAL::` qualifier resolves through the pseudo-package,
# and raku then names the symbol without its `&` sigil.
throws-like 'GLOBAL::index("hello", "l")', X::AdHoc,
    message => /"Could not find symbol 'index' in 'GLOBAL'"/,
    'GLOBAL::<builtin> is not the builtin either';

throws-like 'GLOBAL::Foo::bar()', X::AdHoc,
    message => /"Could not find symbol 'bar' in 'GLOBAL::Foo'"/,
    'a GLOBAL::-qualified unknown package keeps the rest of the qualifier';

# Regression guards: the strip is load-bearing and must keep resolving what it
# was there for.
{
    module M2 { our sub f() { 42 } }
    is M2::f(), 42, 'a qualified user sub still resolves';
}

{
    module Outer { module Inner { our sub g($x) { $x * 2 } } }
    is Outer::Inner::g(21), 42, 'through more than one package level';
}

{
    module M3 { our proto p(|) {*}; our multi p(Int $x) { "int" }; our multi p(Str $x) { "str" } }
    is M3::p("x"), 'str', 'a qualified multi still dispatches';
}

{
    module M4 { our sub h() { 7 } }
    my $name = 'M4::h';
    is EVAL($name ~ '()'), 7, 'and still resolves when reached through EVAL';
}

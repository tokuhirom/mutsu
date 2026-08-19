use v6;
use Test;

# A qualified private-method call `$obj!Owner::method` where `Owner` is
# written short (as `Renderer`, not `Outer::Inner::Renderer`) must resolve
# against the caller's enclosing package chain, the same way an ordinary
# bareword type reference does. `validate_private_access_in_expr()` in
# src/runtime/registration.rs used to string-compare the raw short owner
# name against the caller's fully-qualified registered name, so a perfectly
# legal self-call written from inside a `module` false-positived as
# "Cannot call private method without permission". Several runtime dispatch
# sites (methods_qualified.rs, methods_instance_ops.rs,
# methods_signature_shaped.rs) had the exact same bug independently and are
# pinned here too.
#
# Note: an untrusted qualified private call is a Raku *compile-time* error
# (raku: "===SORRY!=== ... Cannot call private method ... because it does
# not trust ..."), not a runtime exception — it fires the moment the
# offending class body is compiled, regardless of whether the method is
# ever invoked. So the negative cases below wrap the whole offending class
# declaration in a `q[...]` string for `throws-like` to EVAL, matching the
# existing pattern in t/qualified-private-method-nested-owner.t; a plain
# `throws-like { ... }` block would abort the whole test file before the
# block ever runs, since the class declarations inside it are compiled
# immediately as part of the surrounding file.

plan 8;

# --- the reduced repro: a module-nested class calling its own private
# method through a short-name qualified self-call ---
{
    module Outer::Inner {
        class Renderer is export {
            method !secret($x) { "secret:$x" }
            method go($x) {
                my $r = self;
                return $r!Renderer::secret($x);
            }
        }
    }
    import Outer::Inner;
    is Renderer.new.go(42), 'secret:42',
        'module-nested self-call with a short qualified owner name is allowed';
}

# --- the fully-qualified spelling must keep working ---
{
    module M1 {
        class A {
            method !s($x) { "s:$x" }
            method go($x) { self!M1::A::s($x) }
        }
    }
    is M1::A.new.go(1), 's:1', 'fully qualified owner name still resolves';
}

# --- a bare top-level class (no enclosing module) must keep working ---
{
    class Top {
        method !s($x) { "s:$x" }
        method go($x) { self!Top::s($x) }
    }
    is Top.new.go(2), 's:2', 'bare top-level owner name still resolves';
}

# --- a genuine cross-class violation must still be rejected ---
throws-like q[
    class ViolA { method !s($x) { "s:$x" } }
    class ViolB { method go($a) { $a!ViolA::s(1) } }
], X::Method::Private::Permission,
    'an untrusted cross-class qualified private call is still rejected';

# --- `trusts` at the top level must still be allowed (regression guard) ---
{
    class TrustB {...}
    class TrustA {
        trusts TrustB;
        method !s($x) { "s:$x" }
    }
    class TrustB {
        method go($a) { $a!TrustA::s(1) }
    }
    is TrustB.new.go(TrustA.new), 's:1', 'top-level trusts still grants access';
}

# --- `trusts` written short inside a module must resolve the trusted class
# through the same package-chain canonicalization as the owner side ---
{
    module M2 {
        class MB {...}
        class MA {
            trusts MB;
            method !s($x) { "s:$x" }
        }
        class MB {
            method go($a) { $a!MA::s(1) }
        }
    }
    is M2::MB.new.go(M2::MA.new), 's:1',
        'module-nested trusts with a short trusted-class name grants access';
}

# --- and the qualified-owner spelling of the same trust pair ---
{
    module M3 {
        class NB {...}
        class NA {
            trusts NB;
            method !s($x) { "s:$x" }
        }
        class NB {
            method go($a) { $a!M3::NA::s(1) }
        }
    }
    is M3::NB.new.go(M3::NA.new), 's:1',
        'module-nested trusts with a fully qualified owner name grants access';
}

# --- an untrusted cross-class call inside a module must still be rejected ---
throws-like q[
    module M4 {
        class UA { method !s($x) { "s:$x" } }
        class UB { method go($a) { $a!UA::s(1) } }
    }
], X::Method::Private::Permission,
    'an untrusted cross-class call inside a module is still rejected';

done-testing;

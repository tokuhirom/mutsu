use Test;

plan 7;

# `trait_mod:<does>` is a real CORE.setting sub (a callable form of the `does`
# mixin operator) that dists like Hash::Restricted and Injector call directly
# from inside a custom `trait_mod:<is>` handler:
#
#   multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) {
#       trait_mod:<does>(v, SomeRole) if $restricted;
#   }
#
# See todo/deep/trait-mod-does-not-callable-sub.md (resolved) /
# news/2026-08/trait-mod-does-callable-sub.md for the investigation.

# --- 1. The builtin exists and genuinely multi-dispatches -----------------
#
# Real raku does NOT run this cleanly: it reports `Ambiguous call to
# 'trait_mod:<does>(Any, Foo)'` because `trait_mod:<does>(Mu:U $doee, Mu:U
# $role)` already exists as a CORE.setting builtin and this candidate
# collides with it. Reproducing the SAME ambiguity (not "Unknown function")
# is the proof the builtin is registered and visible to multi-dispatch.
{
    my $err;
    try {
        CATCH { default { $err = $_ } }
        EVAL q:to/RAKU/;
            class Foo1782 { }
            multi sub trait_mod:<does>(Mu \v, Mu \r) is export {
                say "would mix {r.^name} into {v.VAR.name}";
            }
            my $x;
            trait_mod:<does>($x, Foo1782);
            RAKU
    }
    ok $err.defined, 'colliding user candidate makes the call ambiguous';
    like $err.message, /:i ambiguous/,
      'the ambiguity error names the builtin/user collision';
}

# --- 2. A non-colliding user extension candidate coexists -----------------
#
# WWW::GCloud::API declares an additional, more specific candidate
# alongside the builtin -- confirm mutsu lets that coexist rather than
# treating any `trait_mod:<does>` declaration as illegal/redeclared.
class ExtraRoleTarget { }
my role ExtraRole { method tag { 'extra' } }
my $extra-called = False;
multi sub trait_mod:<does>(ExtraRoleTarget:U \doee, ExtraRole:U \r) is export {
    $extra-called = True;
}
trait_mod:<does>(ExtraRoleTarget, ExtraRole);
ok $extra-called, 'a more-specific user candidate coexists with the builtin and dispatches';

# --- 3. Writeback: mixing via trait_mod:<does> reaches the caller's variable
#
# The Hash::Restricted/Injector idiom: a `Variable:D \v` capture parameter
# reflects the caller's own variable, and `trait_mod:<does>(v, Role)` called
# from inside the `trait_mod:<is>` handler must mix the role into the SAME
# container the caller declared -- not a detached copy local to the handler.
my role Loud {
    method greet { "LOUD greeting" }
}
multi sub trait_mod:<is>(Variable:D \v, Bool:D :$loud!) is export {
    trait_mod:<does>(v, Loud) if $loud;
}

my %h is loud;
ok %h ~~ Loud, 'the mixin performed inside trait_mod:<is> reached the caller %h';
is %h.greet, 'LOUD greeting', 'the mixed-in role method is callable on the caller %h';

# A second, independently-declared variable must not share the mixin.
my %plain;
nok %plain ~~ Loud, 'an unrelated hash is not affected by another variable mixin';

# --- 4. `nextsame` inside a role mixed into a native value reaches the real
# native method (a bug found and fixed alongside trait_mod:<does> -- a role
# mixed into a builtin Hash/Array/Str whose override calls nextsame used to
# return Nil instead of falling through to the native implementation).
my role Tracer {
    method AT-KEY(::?CLASS:D: \key) {
        nextsame;
    }
}
my %t = a => 1, b => 2;
%t does Tracer;
is %t<a>, 1, 'nextsame inside a mixed-in role AT-KEY falls through to the real Hash AT-KEY';

# vim: expandtab shiftwidth=4

use Test;

plan 8;

# Real raku only auto-adds `*%_` to a signature-less method, never `*@_`:
# `raku -e 'class A { method m { @_.raku.say } }'` => "===SORRY!=== ...
# Placeholder variables (eg. @_) cannot be used in a method. Please specify
# an explicit signature, like method m (*@_) { ... }". mutsu used to
# silently auto-insert an implicit `*@_` for a signature-less method body
# that reads `@_` directly, letting `class B { method m { @_.elems } };
# B.new.m(1,2,3)` return 3 instead of erroring.
#
# mutsu raises this when the method actually runs rather than at compile
# time (matching how the do{}-nested sibling shape already behaves --
# t/placeholder-named-in-method-do.t), so every probe here calls the method.
#
# Every probe below uses the BLOCK form of throws-like (`{ ... }`), not the
# string form (`'...'`): the string form runs its own EVAL-time
# undeclared-variable pre-check (check_eval_undeclared_vars), which does not
# yet know methods get an implicit `*%_`/`*@_` scope and flags @_/%_ as
# X::Undeclared before this fix's die-body is ever reached -- a separate,
# pre-existing gap (see todo/tickets/eval-undeclared-check-blind-to-implicit-method-slurpy.md),
# not something this fix regresses (`%_` already had the exact same problem
# through the string form, unrelated to @_).

# Direct `@_` in a class-body method, called with matching (zero) arity.
throws-like { class B { method m { @_.elems } }; B.new.m },
    X::Placeholder::Block,
    'direct @_ in a class-body method is rejected (called with 0 args)';

# Same shape, called WITH positional args -- the implicit `*@_` insertion
# still lets it bind (so the arity check does not mask the real diagnosis
# with a less informative "too many positionals" error).
throws-like { class B2 { method m { @_.elems } }; B2.new.m(1, 2, 3) },
    X::Placeholder::Block,
    'direct @_ in a class-body method is rejected (called with args too)';

# A submethod is affected the same way as an ordinary method.
throws-like { class C { submethod m { @_.elems } }; C.new.m(1, 2) },
    X::Placeholder::Block,
    'direct @_ in a submethod is rejected';

# `%_` alone (no `@_`) still works: methods DO get an implicit `*%_`.
{
    class D { method m { %_.elems } }
    is D.new.m(a => 1, b => 2), 2, '%_ directly in a method body still works (implicit *%_)';
}

# An explicit signature opts out of the auto-detection entirely -- `@_` is
# then just an alias into the declared slurpy, exactly like a sub.
{
    class E { method m(*@x) { @_.elems } }
    is E.new.m(1, 2, 3), 3, 'an explicit signature makes @_ usable again (opts out of auto-detect)';
}

# `@_` nested inside a `do {}` in a method body is a pre-existing, already
# -fixed sibling case (t/placeholder-named-in-method-do.t) -- pinned again
# here for completeness alongside the direct-usage shapes above.
throws-like { class F { method m { my $r = do { @_ }; $r.elems } }; F.new.m },
    X::Placeholder::Block,
    '@_ nested in do {} inside a method is still rejected (sibling shape)';

# A plain sub is NOT affected: subs DO get an implicit `*@_`.
{
    sub f { @_.elems }
    is f(1, 2, 3), 3, 'a plain sub (not a method) still auto-gets @_ normally';
}

# A role method's direct `@_` usage is unaffected by this fix (role bodies
# never got the class-body walker's auto-detection to begin with --
# `role_method_auto_positional_slurpy_not_applied` pins that at the
# compiler level); it already surfaces as an arity mismatch rather than a
# silent success, so there is nothing regressed here.
throws-like { role R { method relay { @_.elems } }; class G does R {}; G.new.relay(1, 2, 3) },
    Exception,
    'role method direct @_ usage still errors (pre-existing, unaffected by this fix)';

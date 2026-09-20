use Test;

# GH #8863: a punned role's `:U:`/`:D:` multi candidates were not
# discriminated by the invocant. `R.method` (the type-object pun) and
# `R.new.method` (the instance pun) reach the role's method table through
# `dispatch_mixin_method_call` (`src/runtime/methods_mixin_dispatch.rs`),
# a route separate from the ordinary class MRO walk that
# `t/oo/role/role-ud-multi-dispatch.t` already covers for `class C does R {}`
# consumption. That route filtered multi candidates with the invocant-blind
# `method_args_match`, so the punned INSTANCE call always picked the `:U:`
# candidate -- including when the invocant was defined.

plan 6;

role Y {
    proto method g(|) {*}
    multi method g(::?ROLE:U:) { 'U' }
    multi method g(::?ROLE:D:) { 'D' }
}
is Y.g,     'U', 'type-object pun: ::?ROLE:U: selected';
is Y.new.g, 'D', 'instance pun: ::?ROLE:D: selected';

# Spelling the invocant with the role's own name instead of `::?ROLE` is the
# same shape with no pseudo-type resolution involved.
role Y2 {
    proto method g(|) {*}
    multi method g(Y2:U:) { 'U' }
    multi method g(Y2:D:) { 'D' }
}
is Y2.g,     'U', 'type-object pun: Y2:U: selected';
is Y2.new.g, 'D', 'instance pun: Y2:D: selected';

# The working control cases stay working: a plain class, and a role
# consumed by a class (`does`), both dispatch through the ordinary MRO walk
# rather than the mixin route and must be unaffected by this fix.
class X {
    proto method f(|) {*}
    multi method f(X:U:) { 'U' }
    multi method f(X:D:) { 'D' }
}
is (X.f, X.new.f).join(''), 'UD', 'plain class: invocant-discriminated multi still works';

role W {
    proto method h(|) {*}
    multi method h(::?ROLE:U:) { 'U' }
    multi method h(::?ROLE:D:) { 'D' }
}
class Z does W { }
is (Z.h, Z.new.h).join(''), 'UD', 'class consumer: invocant-discriminated multi still works';

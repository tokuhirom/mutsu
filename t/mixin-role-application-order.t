use Test;

# `Value::Mixin(inner, mixins)` stores composed roles keyed
# `__mutsu_role__{Name}` in a plain HashMap, which carries no
# application-order information on its own. `dispatch_mixin_method_call`
# resolved a method-name collision between two mixed-in roles by walking the
# role names in ALPHABETICAL order, not application order -- but Raku's real
# rule is later-wins: `(0 but A) but B).m` answers from B (the most recently
# applied role), regardless of name spelling.
# (todo/tickets/mixin-role-order-not-tracked.md)

plan 5;

role A { method m { "A" } }
role B { method m { "B" } }
is ((0 but A) but B).m, "B", 'later-applied role B wins over A (alphabetical order agrees by accident)';
is ((0 but B) but A).m, "A", 'later-applied role A wins over B (alphabetical order would disagree)';

role Z { method m { "Z" } }
is ((0 but A) but Z).m, "Z", 'later-applied role Z wins over A';
is ((0 but Z) but A).m, "A", 'later-applied role A wins over Z';

# `eqv`/`is-deeply` must not see the internal application-order bookkeeping:
# two separately-built values with the same composition are still eqv.
role Meows {}
is-deeply ((1..5) but Meows), ((1..5) but Meows),
    'two separately-composed but-mixed values with identical composition are eqv';

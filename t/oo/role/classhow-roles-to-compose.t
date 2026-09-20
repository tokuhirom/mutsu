use Test;

plan 3;

# #8845: AttrX::Lazy's `LazyAttributeContainerHOW.compose` calls
# `type.^roles_to_compose` to warn about a name collision with a role queued
# for (native) composition. `roles_to_compose` was entirely unimplemented
# ("No such method 'roles_to_compose' for invocant of type
# 'Perl6::Metamodel::ClassHOW'"), a separate MOP gap from the compose-hook
# ordering bug #8845 is mainly about. Verified against `raku`: even a class
# that DOES compose a role reports an empty `roles_to_compose` once
# composition has finished (it lists roles still queued, not already-applied
# ones) -- mutsu has no such intermediate queued state, so it always answers
# empty, matching the observable behavior at every point user code can call it.

class NoRoles { }
is-deeply NoRoles.^roles_to_compose, (),
    'a plain class reports no roles queued for composition';

role SomeRole { }
class ComposesARole does SomeRole { }
is-deeply ComposesARole.^roles_to_compose, (),
    'a class that already composed a role also reports none queued (matches raku)';

my role ChecksRolesToCompose {
    method compose(Mu \type) {
        state $seen;
        $seen = type.^roles_to_compose;
        type.^add_method('seen-roles-to-compose', method (Mu:D:) { $seen });
        callsame;
    }
}
multi trait_mod:<is>(Attribute:D $attr, :$checks-roles-to-compose!) {
    my $class := $attr.package;
    unless $class.HOW ~~ ChecksRolesToCompose {
        $class.HOW does ChecksRolesToCompose;
    }
}
class ChecksIt {
    has $.x is checks-roles-to-compose;
}
is-deeply ChecksIt.new.seen-roles-to-compose, (),
    'a custom compose override can call .^roles_to_compose without dying (AttrX::Lazy shape)';

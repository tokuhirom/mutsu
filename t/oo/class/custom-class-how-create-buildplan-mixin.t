use lib 't/lib';
use Test;

# A role mixed into the metaobject of a class declared under a superseding
# user HOW (a `class CustomClassHOW is Metamodel::ClassHOW` instance, so the
# mixin reblesses the instance) has its `create_BUILDPLAN` and
# `publish_method_cache` called as the class composes, and may read and
# rebind the HOW's `@!BUILDPLAN` (AttrX::Mooish).

plan 5;

use SupersedeBuiltinHOW;
use nqp;

my @calls;
role PlanHook {
    method create_BUILDPLAN(Mu \type) {
        @calls.push: 'create_BUILDPLAN ' ~ type.^name;
        callsame;
        my \plan := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN');
        @calls.push: 'plan is ' ~ (nqp::islist(plan) ?? 'a list' !! 'not a list');
    }
    method publish_method_cache(Mu \type) {
        @calls.push: 'publish_method_cache ' ~ type.^name;
        nextsame;
    }
}

multi trait_mod:<is>(Mu:U \type, :$planned!) {
    type.HOW does PlanHook;
}

class Planned is planned {
    has $.x = 5;
}

is Planned.HOW.^name, 'CustomClassHOW+{PlanHook}', 'the HOW is the reblessed mixin instance';
is @calls[0], 'create_BUILDPLAN Planned', 'create_BUILDPLAN of the mixed-in role ran';
is @calls[1], 'plan is a list', '@!BUILDPLAN is readable through the HOW';
ok @calls.grep('publish_method_cache Planned'), 'publish_method_cache ran';
is Planned.new.x, 5, 'the class still constructs';

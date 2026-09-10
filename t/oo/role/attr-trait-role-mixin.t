use v6;
use lib 't/lib';
use Test;
use AttrOptTrait;
use AttrNameTrait;

plan 7;

# The JSON::Name shape: a custom trait_mod:<is> mixes a role into the
# Attribute meta-object and assigns one of the role's rw attributes.
# Three separate defects used to break it (each pinned here):
#  1. the mixin/assignment happened on an ephemeral Attribute object and was
#     dropped — ^attributes never saw it;
#  2. inside a module's trait sub, the short role name (`NamedAttribute`)
#     failed to resolve to `AttrNameTrait::NamedAttribute`, so `does` silently
#     degraded to a boolean conformance check and REBOUND `$a` to True;
#  3. two modules exporting trait_mod:<is> candidates collided on the same
#     multi key at import — the second `use` overwrote the first's candidates
#     ("No matching candidates for proto sub: trait_mod:<is>").

# In-file trait (defect 1 alone):
my role Tagged {
    has Str $.tag is rw;
}
multi sub trait_mod:<is>(Attribute $a, Str :$tagged!) {
    $a does Tagged;
    $a.tag = $tagged;
}
class InFile {
    has Bool $.b is tagged("from-in-file");
}
is InFile.^attributes[0].tag, "from-in-file",
    'in-file custom trait: role mixin + value survive to ^attributes';

# Cross-module trait (defects 1+2), with BOTH modules imported (defect 3):
class C {
    has Bool $.b is json-name("isDeprecated");
    has Str  $.o is opted;
}
is C.^attributes[0].json-name, "isDeprecated",
    'imported custom trait: mixin value survives to ^attributes';
ok C.^attributes[0] ~~ AttrNameTrait::NamedAttribute,
    'the attribute smartmatches the mixed-in role';
ok C.^attributes[1] ~~ AttrOptTrait::OptedIn,
    'the other module trait applied to the sibling attribute';
nok C.^attributes[1] ~~ AttrNameTrait::NamedAttribute,
    'roles do not leak across attributes';

# Both traits on ONE attribute (candidates from both modules dispatch):
class D {
    has Str $.x is opted is json-name("xx");
}
ok D.^attributes[0] ~~ AttrOptTrait::OptedIn, 'first trait of two applied';
is D.^attributes[0].json-name, "xx", 'second trait of two applied';

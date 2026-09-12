use v6;
use Test;

plan 2;

# A class multi method should replace a same-signature non-multi method
# composed from a role. Invocant smiley differences distinguish two multi
# candidates, but do not make a role default method coexist with its class
# override.
role RoleDefault {
    method describe() { 'role' }
}

class ClassOverride does RoleDefault {
    multi method describe(::?CLASS:D:) { 'class' }
}

is ClassOverride.new.describe, 'class',
    'the class multi method overrides the role default';
is ClassOverride.^find_method('describe').candidates.elems, 1,
    'the replaced role method is not left as an ambiguous candidate';

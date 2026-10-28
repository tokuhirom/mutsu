use Test;

# `Metamodel::Archetypes.new` takes the flags as named arguments.
plan 4;

my constant ARCH = Metamodel::Archetypes.new: :nominal;
ok ARCH.nominal, 'the named flag is set';
nok ARCH.composable, 'an unnamed flag reads False';
nok ARCH.parametric, 'so does another';

my class MinimalHOW {
    method archetypes(::?CLASS:_: $? --> ARCH) { }
}
ok MinimalHOW.new.archetypes.nominal, 'a constant object as return type';

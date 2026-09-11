unit module EnumKeyScalarDefs;

# Deliberately declares enum keys that collide with very common scalar names
# (`$s`, `$r`, `$px`). mutsu stores a scalar `$s` under the sigil-less env key
# `s`, so an enum key installed under its own bare name used to land in the very
# same slot (#7914).
our Str enum ColliderUnits is export(:ColliderUnits) « :ms<time> :s<time> :px<length> »;

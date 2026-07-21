use Test;

# The `returns` / `of` sub traits must accept a parametrized type name such as
# `Array[Int]` or `Maybe[Array]`. Regression: the trait-type-name parser only
# handled a bare name plus an optional `(...)` coercion source, so it stopped at
# the `[` and left `[Int] { ... }` behind, failing to parse. The `-->` form
# already handled parametrization. (Clu::Command dist uses `returns Maybe[Array]`.)

plan 5;

sub r-plain() returns Array[Int] { my Int @a = 1, 2; @a }
is r-plain(), [1, 2], "returns Array[Int]";

sub o-plain() of Array[Int] { my Int @a = 3, 4; @a }
is o-plain(), [3, 4], "of Array[Int]";

# Nested parametrization
sub nested() returns Hash[Str] { my Str %h = a => 'x'; %h }
is nested()<a>, 'x', "returns Hash[Str] (nested type param)";

# `of` chaining onto a preceding role type still works (no regression)
sub chained(--> Positional) { }
ok chained.defined.not, "of-chaining path unaffected";

# `returns Str()` coercion source still works (no regression)
sub coerce() returns Str() { 42 }
is coerce().^name, 'Str', "returns Str() coercion still works";

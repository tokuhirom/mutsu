# A nested block's `my` in a `use`d unit class file is no longer published as a static

A `my` declared inside a block at the top of a `use`d `unit class` file
(`for @rows { my $op = $_ }`) stayed bound in the loader's env after the body,
and the enclosing `unit class` then published it as one of its statics. A method
call that ran in that package rebound a caller's same-named lexical, so
Math::Symbolic's `.expand` printed `a*(a*a)` instead of `a*a*a`. The class body
now unbinds the names its nested blocks declared. Math::Symbolic `t/01-basics.t`
passes 11/11.

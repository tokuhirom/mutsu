# Regression test: a role pun with named-parameter defaults passes its own type checks

The default curry of `role R[::T = Any, :$flag = 1]` is registered under a name
that carries the named parameters. #15785 made a type check take that very
spelling as the type; this adds the regression test for the `--> R` return
check that `Tree::Binary` hits (`Tree::Binary::Role::BinaryTree.from-Str`,
#15769), covering the instance, the type object and the smartmatch.

# A sub closing over a `:=`-bound List iterates it

`my $list := (1, 2, 3); sub f { for $list { ... } }` ran the body once inside `f` and three
times in the declaring scope; rakudo runs it three times in both. The same went for an Array
(`my $arr := [4, 5]`), a Seq, and a `module` block's `:=` binding read by one of its routines.

Whether `for $x` iterates once (a Scalar item) or element-wise is decided when the `for` is
compiled, from the set of container-less `:=` bindings the compiler has seen
(`noncontainer_bound_vars`). A routine or closure body is compiled by a child compiler that
started with an empty set, so a captured bound scalar looked like an ordinary Scalar. The child now
inherits its enclosing compilers' bindings (`enclosing_noncontainer_bound_vars`) and consults them
only for a name it has not declared itself, so a parameter, `my`, or loop variable of the same name
still shadows the captured one. A `my $copy := $captured` inside the routine stays container-less
as well.

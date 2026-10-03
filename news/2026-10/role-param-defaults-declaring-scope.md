# Parametric role defaults bind in the role's own scope

Tree::Binary's test suite now passes. Its role is
`role BinaryTree[::ValueType = Any, Renderer :$gist-renderer = PrettyTree,
Renderer :$Str-renderer = BasicStrRenderer, ...]`, and two bugs stood in the
way:

- A parameter default was evaluated in the scope of whoever composed the role
  (`class IntTree does BinaryTree[Int]` in a test file). So it could not see
  the classes the role's module imported (`PrettyTree`) or declared in its own
  package (`BasicStrRenderer`). The type check then failed, and composition
  died with "No matching candidate found for the parametric role". Binding the
  parameters now enters the role's declaring compilation unit and package, and
  its captured lexicals. This applies to composition and to a pun
  (`BinaryTree.from-Str(...)`).
- A named argument holding a type object was named after the Pair's `Str`
  (`R[Any,g\t]`), so the role's `::?CLASS` could not be resolved again. Every
  `::?CLASS:U` invocant check on such a pun failed. A named argument is now
  spelled `:g(B)` in the role's name.

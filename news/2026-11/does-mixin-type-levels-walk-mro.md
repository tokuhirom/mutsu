# A mixin type gets a level per `does`, and `.WALK` and `.^mro(:roles)` see it

Groundwork for making every `$obj but R` a real mixin type (#17890), checked
on the `$obj does R` path, which already reblesses the object:

- Every `does` adds a level of its own, as in Rakudo, even for a role the
  class already composes: `$o does R; $o does R` is `C+{R}+{R}`, and
  `D does R; $d does R` is `D+{R}` (it stayed `D` before).
- `.^mro(:roles)` lists a role at each level that composes it
  (`D+{R},R,D,R,Any,Mu`), still once per level for a role reached through
  another one.
- `.WALK` on an object reblessed into a mixin type visits the mixed-in roles'
  methods ahead of the class chain, as it already did for the `but` wrapper.
- A mixin type is no longer reused for a role redeclared under the same name
  (a `my role` declared in a loop body): the cached `C+{R}` records the
  `role_id`s it was composed from and is recomposed when they changed.

Still open on #17890 before `but` can take the same path for every role: a
role attribute named like one of the class's own shares the class's slot, a
renamed mixin's `.^mro` gist shows the old name, and the copy runs its
`DESTROY` at teardown.

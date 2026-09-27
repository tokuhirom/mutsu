# Tinky's state-machine suite runs under mutsu

`Tinky` 0.1.5, a workflow/state-machine library, went from 2 of 8 baseline
test files to all 8 passing locally (the `060-callbacks` file is slow under a
debug build but completes). Eight separate interpreter gaps were in the way,
each fixed generally and pinned by its own `t/` test:

- `Signature ~~ Signature` now compares parameter types through the type
  registry, so `:(ObjectOne $) ~~ :(Object)` holds when `ObjectOne` does the
  user role `Object` (previously only built-in types were known).
- A role built through the MOP — `Metamodel::ParametricRoleHOW.new_type`,
  `.^add_method`, `.^set_body_block`, `.^compose` — is a real role: its methods
  reach whatever composes it, and its body block runs at each composition with
  the consuming type.
- `.^mixin` reblesses the object in place, exactly as `does` does, so
  `self.^mixin($role)` inside a method changes the invocant itself.
- The class method `Supply.merge(@supplies)` flattens its array argument.
- A Proxy whose FETCH ends in an `is rw` routine call is read as the value in
  that routine's container, so a chained method call on it works.
- Private-method access is lexical: a closure written in a role method can call
  the role's private methods even when another class's method invokes it, and a
  closure created in a role method resolves the role's module-internal names.
- A class's `multi method ACCEPTS` candidates now combine with the inherited
  core ones in smartmatch, so a topic none of them binds falls back to identity
  instead of dying with "Cannot resolve caller".
- `$x ~~ $_` with a bare `$_` right-hand side no longer writes the enclosing
  topic into `$x` (`@list.grep({ $obj !~~ $_ })` used to overwrite `$obj`).
- `grep` fetches a Proxy matcher once, and roles a method trait composes onto a
  method (`$m does Tag`) are visible on the method objects `.^methods`,
  `.^find_method` and `.^lookup` return.

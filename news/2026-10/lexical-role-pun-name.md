# A punned lexical role is named after the role

`my role TC {}; TC.new.^name` was `TC+{TC}`; it is `TC`, as for a package role. The pun excluded
the role equal to its own base from the `+{...}` suffix by raw name, but a lexical role is keyed
by its mangled name, so the comparison missed; it now compares the display name too. The same
mangled key made `has @.a is TC` (a container-type trait naming a lexical role) report the
mixin name.

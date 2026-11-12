# A role method's binding names are interned once

`apply_method_role_bindings` put a parametric role's composition bindings
(`T` and the role-body lexicals) into each call's env by name, interning each
name again on every call. `MethodDef` now keeps them interned alongside the
bindings list they came from (the composer replaces that list on cloned
defs, so a def whose list changed falls back to interning). A trivial role
method call (`$maybe.maybe-type`) went from ~18.5K to ~17.2K instructions
(#17331).

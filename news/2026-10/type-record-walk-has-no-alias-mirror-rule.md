# A typed variable's record walk loses its alias-mirror rule

ADR-13757 step 4 made a routine's record walk end at its own frame root. The
remaining special case, `leaves_variable` / the `bound_here` bookkeeping in
`Env::walk_with_fallback`, only decided whether a caller frame's binding was
an alias mirror of the one below it. The sigilless alias parameter now carries
its source's record under its own name (`alias_param_declared_type`), so the
rule had no live shape left. An ablation of it left all 9251 `t/` files and the
1428-file roast whitelist green, including `t/collections/buf-typed-element-width.t`,
the NativeCall shape the previous slice kept it for. It is deleted;
`same_variable` stays for the static-link rule.

# A dynamically named class no longer trips the EVAL undeclared-name check

`class ::($name) { }` declares a name only the run-time value knows, so `check_eval_undeclared_names`
could not see it and rejected `DynClassA.value` in the same `EVAL`'d unit with `Undeclared name`
(rakudo accepts it). A unit with a dynamically named sub or class is now not judged, as the mainline
check already did for subs. `t/vm/binding/indirect-declarator-names.t` joins the `EVAL($ast)` ratchet.
Found by the #16356 triage.

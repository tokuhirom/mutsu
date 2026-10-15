# An augmented slurpy `multi` on `Any` ranks after the built-in method for a user-class instance too

`augment class Any { multi method map(|c) {...} }` joins the built-in `Any.map` candidates and, being a
bare capture, ranks after them. #13510 did this for core-type receivers (`List`, `Int`, ...); a user-class
instance (`class C {}; C.new.map(*.so)`) still ran the augmented candidate, because three gates asked
"has a user method of that name" without the ranking rule: the VM's native-call gate
(`skip_native`), the compiled-method resolver and `call_method_with_values`' `shadows_builtin`. They now
share `user_candidates_all_outranked_by_builtin`, so the instance falls through to the built-in method
exactly as `raku` does. The receiver restriction (`is_core_type_receiver`) is gone.

Fixes #13509. Pinned by `t/oo/method/multi-method-capture-vs-core-candidates.t`.

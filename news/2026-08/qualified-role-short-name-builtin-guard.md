# A qualified role no longer steals a built-in type's short name

Declaring `role Cro::HTTP::Middleware::Pair { }` anywhere in a process made the
bare name `Pair` resolve to that role. `("a" => 1) ~~ Pair` became `False`,
`$p.WHAT =:= Pair` became `False`, and every `when Pair` in every module
silently fell through to `default`.

A `class`/`role` declared with an already-qualified name registers a short-name
alias in the env, so that code inside `unit module M` can refer to its own
`M::R1` as bare `R1`. `exec_register_class_op` has always skipped that alias
when the short name is a built-in type — the comment there cites
`my class X::Roast::Channel` not being allowed to capture bare `Channel`.
`exec_register_role_op` grew the same alias without the guard, so roles could do
exactly what classes were forbidden from doing.

The role path now shares the guard (`!Self::is_builtin_type(&short)`). The alias
still installs for a non-built-in short name, which is the case it exists for.

Two things made the fallout wide. `hoist_type_decl_shells` emits a
`RegisterRole` shell for every non-lexical role at the *head* of the compilation
unit, so the poisoning alias landed before the mainline ran — a role declared
textually below a `when Pair` still broke it. And bareword resolution consults
the env before the built-in types (`exec_var_get_op` returns a `Package` whose
name differs from the bareword), so the alias won every lookup.

Found while running the upstream `Cro::HTTP` suite: `Cro::HTTP::Router`'s
`delegate` died with `Must pass one or more Pairs to 'delegate', not a Pair` —
a message that is only possible when `.^name` says `Pair` and `when Pair`
disagrees. `Cro::HTTP::Middleware::Pair` was the role in question.

Pinned by `t/qualified-role-does-not-shadow-builtin.t`, which passes identically
under raku.

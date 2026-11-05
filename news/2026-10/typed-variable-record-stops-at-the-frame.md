# A variable's type record no longer walks into the frames that called the routine

ADR-13757 step 4. The declared-type records of a variable (`__mutsu_type::x`,
the hash-key record) were looked up through the env chain and, past the running
routine's frame, through the frames that called it: a caller's `my Int @x` could
answer for the routine's own free `@x`. Earlier slices made the declared slots
read the running frame only; the remaining by-name reads still took that walk.

The walk now ends at the frame root of a routine that has a lexical outer of its
own, where the static link already says which frames the variable can come from.
A block or closure frame, whose outer is the frame that ran it, keeps the walk.
The one legitimate caller-frame read was an alias: a sigilless parameter `\x`
bound to `@q` stores through `x` and is checked against `@q`'s type. The binder
now writes the type the argument's `WrapVarRef` site resolved in the caller's
own frame under the parameter's name and under the variable's
(`alias_param_declared_type`), so the alias no longer asks the caller.

Measured before the change by stopping the walk at the origin frame: `prove -r
t` failed two files (the alias, and `MY::<$x> = ...` inside a block), the roast
whitelist none; after it both pass, and so does the whole of `t/` and the roast
whitelist (apart from the files in `ci/known-env-failures.toml`). Pinned by
`t/routines/signature/sigilless-param-keeps-source-array-type.t`.

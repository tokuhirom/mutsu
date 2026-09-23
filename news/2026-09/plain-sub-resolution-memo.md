# A plain sub that misses the light call paths no longer re-resolves its name on every call

[#9081](https://github.com/tokuhirom/mutsu/issues/9081). JSON::Fast's
`unjsonify-string` declares an inner `my sub`, which keeps it off the
positional-light and OTF call paths. Every call then went through
`dispatch_func_call_inner`, and `MUTSU_VM_STATS=1` counted three full
`resolve_function_with_types` walks of the same name per call.

## The diagnosis in the issue was not the cause

The issue suspected the `Uni:D` smiley on `unjsonify-string`'s first
parameter of making the lone candidate "value-dependent" and so uncacheable.
It does not: `type_constraint_is_value_dependent` already peels a `:D`/`:U`
smiley, and `multi_arg_type_keys` carries definedness in the key. Two other
gates refused the name:

- `func_multi_dispatch_type_cacheable` only counts *arity-keyed* candidates
  (`GLOBAL::name/2`). A plain `sub` is registered under `GLOBAL::name` alone,
  so its candidate list is empty and the name is "not cacheable" -- every
  plain sub, not only smiley-typed ones. `sub h(Int:D $x)` in a plain script
  resolved in full once per call too.
- `unjsonify-string` is also a compunit-private helper of the module, and both
  that gate and the VM's `fn_resolve_cache` refuse such a name outright,
  because it resolves differently depending on which compunit is asking.

The three walks came from `find_compiled_function_memo`, from
`user_function_matches_call`, and from the resolve that fetches the def to
compile it -- the call does not find the routine in the caller's compiled
table and takes the builtin-shadow branch.

## The fix

When no `multi` candidate is in scope, the bare-name resolver answers with the
running compunit's private routine or with the first package in the search
chain that registers `Pkg::name` exactly. Neither step reads the arguments, so
`resolve_function_with_types` now memoizes that answer
(`src/runtime/plain_fn_resolve_memo.rs`), keyed by name, `current_unit`,
current package, innermost lexical package and the proto generation, and
tagged per entry with `fn_resolve_gen` like the other dispatch memos. Every
caller of the resolver gets it, so all three walks collapse at once.

Two answers are deliberately never recorded: a compunit-private routine found
through the *executing frame's* unit rather than `current_unit` (the key does
not carry the frame), and a package-searched routine whose name is private to
any compunit (a later frame could need the private one). Negative answers are
not recorded either, since the multi walk after them reads the arguments.

## Measured

`MUTSU_VM_STATS=1`, `from-json` of an array of 110 `"a/b"` strings (the #9073
repro shape): `function-full-resolve` for `unjsonify-string` goes from 330 to 1
and for `parse-string-slow` (an `int $pos is rw` plain sub on the same path)
from 221 to 1.

Wall clock, release builds side by side, both warmed first, mean of 7 paired
runs of the same repro with 20000 strings: 2447 ms before, 2359 ms after
(about 3.6%).

While writing the regression test, a user `sub caller` turned out to receive
mutsu's internal `__callframe_line` named argument and die; that is
pre-existing and filed as [#9093](https://github.com/tokuhirom/mutsu/issues/9093).

The structural direction the issue also names -- binding an inner sub as a
frame lexical so `has_inner_subs` no longer excludes the body from the light
paths -- is not part of this change.

Pinned by `t/routines/dispatch/plain-sub-dispatch-resolution-memo.t` and unit tests in
`plain_fn_resolve_memo.rs`.

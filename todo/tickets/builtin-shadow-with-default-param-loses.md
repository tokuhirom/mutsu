# A user sub that shadows a builtin loses the call when it has a default parameter

Found 2026-07-25 while fixing the sigilless-parameter topic-rebinding bug for
`String::Rotate` (`TODO_dist` T-057). That fix made the dist's *method* half pass;
this is the remaining half, and it is a different, broader root cause.

## Repro

`tmp/mlib/MyRot.rakumod`:

```raku
unit module MyRot;
sub rotate (Str $s, Int $n = 1 --> Str) is export { "R:$s/$n" }
sub abs    (Str $s, Int $n = 1 --> Str) is export { "A:$s/$n" }
```

```raku
use MyRot;
say rotate('x', 3);   # raku: R:x/3   mutsu: Nil
say abs('x', 3);      # raku: A:x/3   mutsu: A:x/3    <- user sub wins
say abs('x');         # raku: A:x/1   mutsu: dies in the BUILTIN abs
```

The pattern is sharp: the user's sub loses **exactly when the call's argument
count matches a native builtin of the same name**. `abs('x', 3)` wins because
there is no 2-arg builtin `abs`; `abs('x')` and `rotate('x', 3)` lose because
1-arg `abs` and 2-arg `rotate` exist natively.

A locally-declared `sub rotate` in the same file wins, so this is not simply
"builtins beat user subs" — it is specific to the shape below.

## Root cause

`vm_call_func_ops.rs` (~line 1247) resolves a single non-proto candidate and, when
the name is a builtin, applies a deliberately **strict** gate before running it:

```rust
let gate_ok = if is_builtin {
    // Genuine builtin shadow: strict gate (no default —
    // name-cache pollution hazard, PR #3546).
    Self::def_is_otf_compilable(&def)
} else {
    Self::def_is_otf_compilable_module_single(&def)
};
```

`def_is_otf_compilable` rejects a signature with a **default parameter**. When it
rejects, control falls through to `vm_call_function_fallback`, which dispatches
the native builtin — silently running the wrong routine rather than reporting
anything. `Int $n = 1` is an extremely ordinary signature, so any module that
exports a builtin-named sub with a default is affected.

## Why it is not a one-liner

The strict gate exists on purpose: PR #3546 found that OTF-compiling a
builtin-shadowing def with defaults pollutes the name-keyed call caches. The fix
is therefore not "relax the gate" but "when the gate rejects, still call the USER
def (through the interpreter path) instead of falling through to the builtin" —
i.e. the fallback needs to know a user shadow exists and prefer it. That touches
the hot named-call path and the caches #3546 was protecting, so it wants its own
PR with a full roast run.

## Affected files

- `src/vm/vm_call_func_ops.rs` — the `gate_ok` branch (~1247-1285) and the
  `vm_call_function_fallback` call that follows it
- `src/runtime/builtins.rs` — `BUILTIN_FUNCTION_NAMES` / `is_builtin_function`,
  which decides that a name is a builtin at all

## Impact

`String::Rotate`'s `t/01-basic.rakutest`: 68 of 136 subtests (every `sub rotate`
assertion; the `method rotate` half passes now). Any dist exporting a
builtin-named sub with a defaulted parameter hits it.

Related, smaller: `&rotate.signature.gist` renders the default as
`Int \ch = ...` where raku renders `Int \ch = 1`.

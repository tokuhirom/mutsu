# `.WHICH`/`.WHY` ignore a user-defined override except via a compile-time-literal quoted method call

## Repro

```raku
class Foo {
    method WHICH { "USER-WHICH" }
}
say Foo.new.WHICH;          # bareword: raku USER-WHICH, mutsu Foo|<hash>  -- WRONG
say Foo.new.'WHICH'();      # quoted literal: raku USER-WHICH, mutsu USER-WHICH -- correct
my $m = "WHICH";
say Foo.new."$m"();         # dynamic (CallMethodDynamic): raku USER-WHICH, mutsu Foo|<hash> -- WRONG
```

Same shape for `.WHY` (`method WHY { "USER-WHY" }` -> raku `USER-WHY` bareword and dynamic, mutsu
`Nil` bareword, `Foo|<hash>`-style native answer via `try_native_method` for dynamic).

The other six MOP pseudo-methods (`DEFINITE`/`WHAT`/`WHO`/`HOW`/`WHERE`/`VAR`) are **not** affected
by this bug — raku itself never consults a same-named user method for those in *any* call form
(verified: bareword `.WHAT`/`.WHO`/`.HOW`/`.DEFINITE`/`.WHERE`/`.VAR` on a class defining
`method WHAT {...}` etc. still returns the true reflection value in real raku, ignoring the user
method entirely — they are genuine compile-time-special MOP macros, not overridable). Only
`WHICH` (the documented mechanism for giving a class custom value-identity semantics) and `WHY`
(the Pod-doc accessor) are real, ordinary, overridable methods in raku regardless of call syntax.
mutsu's quoted-literal call form (`.'WHICH'()`/`."WHICH"()` with a compile-time-known string)
already gets this right; every other call form does not.

## Root cause: two independent, redundant "skip native pseudo dispatch" mechanisms, neither aware that WHICH/WHY are exceptions

**Mechanism 1 (VM opcode level, `skip_native`).** `exec_call_method_op_impl`
(`src/vm/vm_call_method_ops.rs:948-953`) and `exec_call_method_mut_op_impl`
(`src/vm/vm_call_method_mut_ops.rs:1126-1130`) each compute a per-call `skip_native: bool` that is
`true` for all 8 pseudo-method names **only when `quoted` is true** (a compile-time flag on the
`CallMethod`/`CallMethodMut` opcode itself, set when the AST shows a literal quoted method name
like `.'WHICH'`/`."WHICH"()`, distinct from `.WHICH` bareword). Separately, the same function's
has-user-method check (`vm_call_method_ops.rs:964-968`, `vm_call_method_mut_ops.rs:1141-1145`)
**explicitly excludes all 8 pseudo names** from ever setting `skip_native` via
`has_user_method`, with the comment "but NOT for pseudo-methods like DEFINITE, WHAT, etc. which
are macros" -- correct for the other six, wrong for `WHICH`/`WHY`. When `skip_native` is `true`,
`self.skip_pseudo_method_native = Some(method)` is recorded
(`vm_call_method_ops.rs:1031-1039`, gated on `quoted && skip_native`;
`vm_call_method_mut_ops.rs:1215-1217`, gated only on `skip_native`).

`exec_call_method_dynamic_op`/`exec_call_method_dynamic_mut_op` (the `.$var`/`."$var"()` runtime-
string forms, same file, `CallMethodDynamic`/`CallMethodDynamicMut`) and
`exec_hyper_method_call_op`/`exec_hyper_method_call_dynamic_op`
(`src/vm/vm_hyper_method_ops.rs`) have **no `skip_native`/pseudo-method concept at all** -- their
general fallthrough calls `self.try_native_method(target, method_sym, args)` unconditionally, so
for these four entries `WHICH`/`WHY` are *always* native-computed regardless of quoting.

**Mechanism 2 (interpreter level, inside `call_method_with_values`).**
`methods_call_dispatch.rs:2770-2802` computes its own `is_pseudo_method` +
`bypass_native_fastpath` (via `should_bypass_native_fastpath`) and separately consumes
`skip_pseudo_method_native` inside `dispatch_method_by_name_1`
(`methods_dispatch_match.rs:20-31`) to let the `WHAT`/`HOW`/`WHO`/`WHY` match arms
(`methods_dispatch_match.rs:199-202`) fall through to normal method resolution instead of
computing the macro value -- but only when the caller (mechanism 1) actually set the flag for
this exact method name. There is no `"WHICH"` arm in `dispatch_method_by_name_1`'s match at all
(nor in the other `dispatch_method_by_name_N` files, per a full grep) -- `WHICH` apparently only
resolves via the generic/native fallback further down `call_method_with_values`, which is where
the actual native `native_method_0arg` "WHICH" arm (`builtins/methods_0arg/dispatch_core_coerce.rs:343`)
gets consulted for the case that reaches the interpreter at all.

## Why this needs a design pass, not a quick patch

1. **`try_native_method_raw` itself has no has-user-method guard for `WHICH`/`WHY`** (its
   Instance-specific bypass block, `vm_native_dispatch.rs:191-265`, guards `gist`/`Str`/`Stringy`/
   `raku`/`perl` via a `render_overridden` check at lines 235-246 but nothing analogous for
   `WHICH`/`WHY`). Adding one there (mirroring `render_overridden`) is necessary to fix the four
   entries (`CallMethodDynamic`/`CallMethodDynamicMut`/`HyperMethodCall`/`HyperMethodCallDynamic`)
   that call `try_native_method` unconditionally with no opcode-level pseudo-method gate at all.
2. **But that alone is not sufficient**, because mechanism 2's `skip_pseudo_method_native` flag
   is a *separate* signal consumed by `dispatch_method_by_name_1`'s WHAT/HOW/WHO/WHY match arms --
   and per the point above, WHICH has no arm there at all, meaning its actual resolution path
   through the interpreter (once `try_native_method` correctly declines) is not yet mapped. This
   needs tracing before a fix can be written with confidence: does `call_method_with_values` fall
   through to ordinary MRO method resolution for `WHICH` once native is declined, or does it hit
   some *other* native-style computation further down that would need its own has-user-method
   guard too?
3. **Five call sites, two independent mechanisms.** A correct, non-duplicated fix likely wants the
   guard to live once in `try_native_method_raw` (mechanism 1's shared self-guard, per
   `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`'s Phase E finding that the
   `Native` candidate's real safety net is always inside `try_native_method_raw`'s own per-shape
   checks, not scattered per-entry) -- but the opcode-level `skip_native`/`quoted` gates
   (mechanism 1) and the interpreter-level `skip_pseudo_method_native` consumption (mechanism 2)
   both need auditing for whether they still serve a purpose after that, or become redundant/
   inconsistent with each other for `WHICH`/`WHY` specifically (they'd stay load-bearing as-is
   for the other six pseudo names).
4. **Verification surface is real but narrow**: `WHICH`/`WHY` overrides are a real, documented
   Raku idiom (custom value-identity types), but likely rare in practice -- worth fixing
   correctly, not worth rushing given the mechanism complexity above.

## Where found

Discovered during ADR-0019 E5c (`CallMethodDynamic` dispatch-ordering classification,
`news/2026-08/adr0019-e5-e7-entry-routing.md` §"E5c") while raku-verifying whether
`HyperMethodCallDynamic`'s missing `skip_native`/`has_user_method` gate (inventory correction 4)
produces observable divergence -- it does, but the same divergence traces back to a pre-existing,
narrower-than-suspected bug (only `WHICH`/`WHY`, not the full MOP pseudo-method set) unrelated to
the E5 native-vs-user routing campaign itself. Not part of that campaign's scope; filed
separately.

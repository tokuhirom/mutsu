# Fast-arity call paths now reject over-supplied positional arguments

Calling a plain, fixed-arity sub or block with more positional arguments than
its signature declares used to be silently accepted on mutsu's "fast" and
"light" call paths -- the extra arguments were simply dropped instead of
raising raku's `X::AdHoc: Too many positionals passed; expected {n} arguments
but got {m}`. The check has existed in the general binder
(`bind_function_args_values` in `src/runtime/types/binding_signature.rs`) for
a long time and fired correctly for any signature that forced a call onto
that path (a named param, a `where` constraint, `multi`, or a `method`) -- the
gap was confined to the fixed-arity fast paths that bind straight into local
slots without going through the general check.

Three distinct call-dispatch sites had the gap, not just the two named in the
originating ticket:

- `call_compiled_function_positional_light` (`src/vm/vm_call_light.rs`): the
  cached fast path for a plain positional-only sub/block signature (e.g.
  `sub g($a) {}`, `sub g2($a, $b) {}`). It already raised "Too few
  positionals" on a shortfall but never checked for a surplus. Since
  `is_positional_light_call_eligible` already guarantees the signature has no
  slurpy/optional/named param, a surplus argument is unconditionally an arity
  error -- no new compile-time flag was needed, just one more integer
  comparison mirroring the existing shortfall check.
- The dedicated zero-arg fast-call cache in `src/vm/vm_call_func_ops.rs`
  (`call_compiled_function_fast`'s call site, `is_fast_call_eligible`
  requires a completely empty signature): a single popped stack value that
  is not the synthetic callsite-line marker Pair is, by construction, a
  genuine over-supplied argument to a zero-param routine.
- The **legacy placeholder binder** in `binding_signature.rs`
  (`bind_function_args_values`'s `param_defs.is_empty()` branch, which binds
  by a bare `params: Vec<String>` list instead of full `ParamDef`s). This is
  where a single-param pointy block (`-> $a { }`, compiled via `Expr::Lambda`)
  and a non-mutating WhateverCode (`*+1`) actually bind their arguments --
  *not* through either fast-path file the ticket named. This branch had a
  standing comment explaining why a "too many" check was deliberately never
  added: a `^`-twigil placeholder sub (`{ $^a }`) whose body also reads a
  bare `@_`/`%_` legitimately accepts more positionals than its placeholders
  declare (the leftover flows into `@_`), and nothing in the raw `params`
  list seemed to distinguish that shape from one that should reject extras.
  Investigation showed the distinguishing signal was there all along: caret
  placeholders (`^a`, `@^arr`, ...) carry a literal `^` prefix in `params`,
  and raku rejects at compile time (`X::Signature::Placeholder`) any attempt
  to combine an explicit pointy-block param with a body `@_`/`%_` read -- so
  a `params` list made entirely of plain (non-caret, non-colon) identifiers
  is *never* ambiguous. The fix adds the "too many" check gated on exactly
  that condition, leaving the documented placeholder/`@_` leniency untouched.

All three now raise the byte-identical `"Too many positionals passed;
expected {n} arguments but got {m}"` wording the general binder already used,
since several call sites (`main_args.rs`, `calls.rs`, `builtins_lvalue.rs`,
`methods_instance_ops.rs`) pattern-match on that exact string.

New test: `t/too-many-positionals-fixed-arity.t`, covering all three fixed
sites plus a WhateverCode positive-binding sanity check.

Fallout was minimal: the local `t/` suite (3294 files) had exactly one
failure, `t/compunit-can-install.t`, and it is an unrelated environment
artifact (the test asserts a synthetic root path is non-writable, which does
not hold in this container's filesystem layout) -- not a consequence of this
change. A 196-file roast sanity sweep across `S06-signature`, `S02-*`, and
binding/multi-dispatch/call-adjacent files was fully clean. This is notably
*less* fallout than the originating ticket anticipated; the two known
pre-existing caret-placeholder oddities surfaced during investigation
(`{ $^a; @_.elems }` and `sub f { [@_[0], @^arr.elems] }` both already
mis-binding on `main`, unrelated to this fix) were left untouched as
out-of-scope, separate bugs.

The ticket that motivated this fix cited `ADR-0054` §2.2's
`g((1,2).Slip)`-on-a-one-param-sub example, where mutsu's then-blind `.Slip`
argument-list flattening silently corrupted a call instead of raising the
loud arity error this fix now provides. That root cause (blind Slip
flattening at call sites) was independently fixed by ADR-0054's own
implementation before this ticket was picked up -- `g((1,2).Slip)` already
binds the whole 2-element Slip as one argument, matching raku -- so the
example no longer reproduces. This fix still closes the general gap ADR-0054
§2.2 warned about: an explicit `|`-spread (`g(|@z)`) over-supplying a
fixed-arity callee now raises the correct arity error on every dispatch path
instead of silently truncating.

# A `multi` call no longer pays the general frame-pushing call path

Calling a `multi sub` cost **8.5x** what calling an identically-shaped plain
`sub` cost, with the same opcode count, the same JIT behaviour and zero
interpreter fallbacks on either side. All of the difference was in the call
machinery a `multi` was routed through.

Measured with callgrind (release, 20 000 calls, deterministic retired
instructions), `multi sub m(Mu $c, $d = '')` against `sub s(Mu $c, $d = '')`,
both called `m(1, "x")` / `s(1, "x")` in a loop:

| | before | after |
| --- | --- | --- |
| plain `sub` call | 5 750 Ir | 5 750 Ir (unchanged) |
| `multi` call | 47 303 Ir | 28 992 Ir |
| the `multi` surcharge over the plain `sub` | 41 553 Ir | 23 242 Ir (**-44.1%**) |
| whole-benchmark instructions | 969.2 M | 602.8 M (**-37.8%**) |

Wall clock on the same loop: 0.847 s -> 0.469 s. (Wall clock on this box wobbles
±13% between runs of the identical binary, which is why the table is retired
instructions; they are exact and load-independent.)

## What the surcharge was

`dispatch_func_call_inner` resolves the winning candidate, then hands it to
`compile_and_call_function_def`, which ran it through
`call_compiled_function_named` — the general, frame-pushing call entry. That
entry was 18 600 of the 41 553 surplus instructions: a `push_call_frame`, a
lazy routine-code push, a scoped-overlay env child, a `$!` reset probe, a `$_`
seed, and the full `bind_function_args_values` binder.

An ordinary `sub` of exactly that signature does not go there. It takes
`call_compiled_function_positional_light`, which binds straight into
precomputed local slots. The two light paths in `dispatch_func_call_inner` are
explicitly skipped for a multi (`!self.has_multi_candidates_cached(name)`), and
that guard is right *there*: those sites populate the name-keyed
`pos_light_call_cache` / `light_call_cache`, and a name-keyed entry would make a
later call with different argument types reuse the first call's candidate.

But `compile_and_call_function_def` populates no such cache — it is reached
*after* `resolve_function_multi_cached` has already picked the winner for these
exact arguments. So the candidate can take the same positional-light entry, with
every other guard that path applies (container-into-scalar sharing, the
`is test-assertion` trait, a `wrap` chain, a mainline-lexical capture) applied
here too, plus one this site needs and the others get for free: the callee must
be in the caller's own **compilation unit**. That is the invariant the light
path is written under — a module's file-scope `my` is lexical to its compunit,
and only the general entry's `is_unit_lexical_of` check keeps a module sub's
write to one off the loading script's same-named lexical
(`t/module-file-scope-lexical.t`). `compile_and_call_function_def` is the entry
an *imported* sub reaches, so without the gate that isolation was lost.

`nextsame`/`callsame`/`samewith` keep working because the multi-dispatch and
samewith frames are pushed *around* this call, not inside the callee entry —
`t/multi-positional-light-dispatch.t` pins that, together with per-call
re-resolution across alternating argument types, `:D`/`:U` discrimination,
defaulted positionals, topic freshness and parameter read-onlyness.

## Two latent holes in the light path the routing change made reachable

Both were real bugs that had simply never been reachable from the call shapes
that used to arrive there.

### An untyped return-type failure

The positional-light path enforces a fast return type itself, and raised a plain
`RuntimeError` for a failure. So `throws-like 'f()', X::TypeCheck::Return` saw
an untyped exception and failed for any routine with a return type that reached
that path (`roast/S02-types/type.t`, eight subtests). It now raises through the
same `throw_type_check_return` the general return-value path uses.

### `Cool` treated as a wildcard

`FastParamType` classified `Cool` alongside `Any` and `Mu` as "satisfied by
every value", and the by-name `fast_type_check` had the same `"Any" | "Mu" |
"Cool" => true` arm. That is wrong: `Cool` is a real type, and a user class
instance does not do it. `t/light-call-type-check-tags.t` has always pinned
`dies-ok { bad-wide(Widget.new) }` for `sub bad-wide(Cool $c)`; it passed only
because that particular call shape never reached a light path. Routing more
calls there exposed it, so `Cool` is no longer a fast type name at all: a
`Cool`-constrained parameter or return type leaves the light paths and is
enforced by the general binder, which consults the class MRO.

## Three allocation/interning leaks on the same path

The remaining surcharge is the *resolution*, and three pieces of it were pure
waste:

- **`find_compiled_function_inner` built the argument type signature twice** —
  a `Vec<String>` with a heap-allocated `String` per argument, once for the
  resolution-cache key and once for the compiled-key probes — and then `clone`d
  it into the key. For a multi name the key is never used at all (`use_cache` is
  false), so every one of those allocations was for a lookup that cannot happen.
  Now the signature is built once and the key only when it will be consulted.
- **`multi_arg_type_keys` re-interned its reserved marker names on every use.**
  It runs twice per multi call (the callsite resolution and
  `resolve_function_multi_cached` each build the key) and emitted up to three
  markers per argument; on the two-argument benchmark it interned 12 names per
  call, 8 of them compile-time constants. Those are now interned once per
  process.
- **`call_compiled_function_named_inner` built `__mutsu_callable_id::<pkg>::<name>`
  with `format!` on every named call** — a heap allocation plus a hash of ~40
  bytes to intern it — for a mapping fixed for the life of the routine. It is
  now memoized per `(package, name)` symbol pair, next to the sibling
  `type_meta_key_for_sym` / `placeholder_key_sym` memos.

## What this does *not* fix

The vendored `Test.rakumod` assertion path is unchanged: 191 347 retired
instructions per `ok 1, "x"` before and after. Its `ok` reaches the callee
through `ExecCallPairs` (the parser attaches a `__mutsu_test_callsite_line`
named argument to every test-assertion call), not through
`dispatch_func_call_inner`, and two thirds of the cost is inside `proclaim`,
whose `Bool(Mu) $cond` coercion and `$desc is copy` trait exclude it from the
light path by construction. That deficit is flat — the largest single entry in
its per-assertion profile is 5.9%, and it is `malloc` — so it is general
interpretation cost, not one hotspot. Recorded on
[#7573](https://github.com/tokuhirom/mutsu/issues/7573).

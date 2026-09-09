# A listop statement call probes the real argument list, and resolves it once

`ok 1, "x"` and `is $got, $expected, "..."` — the shape every roast assertion
is written in — compile to `OpCode::ExecCallPairs`, because the parser injects
a `__mutsu_test_callsite_line` **named** argument into every test call and a
statement-level listop call with a named argument is exactly what that opcode
is for. Its handler had three arms: the caller's compiled table
(`find_compiled_function`), a Rust builtin (`try_native_function`), and a
carrier fallback into `exec_call`.

Two things were wrong at that entry, and they compounded:

1. **Both probes ran against the unsanitized argument list.** The marker pair
   is stripped by `sanitize_call_args_owned`, which only ran *inside* the
   carrier arm — so the two arms above it asked "is there a routine matching
   `ok(Bool, Str, __mutsu_test_callsite_line => 3)`", a call shape that does
   not exist. `OpCode::ExecCall`, the positional-only sibling, has always
   sanitized first; `ExecCallPairs` never did.
2. **The resolution was then thrown away.** `find_compiled_function` resolves
   the winning candidate on its way to answering "is it in *this* table", and
   `exec_call` opens by resolving the same call again through
   `resolve_function_with_alias`. Both go through
   `resolve_function_multi_cached_keyed`, so every listop call that reached the
   carrier resolved its routine twice.

The handler now sanitizes once at the top — threading the extracted callsite
line to the carrier explicitly, so a failing assertion still reports its own
source line — and hands the resolved winner over to `exec_call_sanitized`,
which uses it in place of its own `resolve_function_with_alias`.

Only a *type-keyed* resolution may be handed over. `find_compiled_function_memo`
fills its memo exactly then, and such an answer is a pure function of
`(package, name, argument type keys)` — which is precisely the licence
`resolve_function_multi_cached_keyed` documents for reusing one resolution
across two consumers (#7573). An un-keyable call still resolves inside
`exec_call_sanitized` as before, because that resolution reads
`pending_call_arg_sources` and two resolutions of it may legitimately differ.

## Measured

`tmp/bench-ok-2k.raku` (`use Test; plan 2000; for ^2000 { ok 1, "x" }`) under
`MUTSU_REAL_TEST=1`, callgrind, release build. The `for` loop subtree, which
is the 2000 assertions:

| | Ir |
| --- | --- |
| before | 381,978,344 |
| after | 379,117,151 |

-2.86 M Ir, -0.75%, i.e. ~1,430 Ir per assertion — the whole of the second
resolution. That is the size #7574 predicted for this arm, and the ticket says
in as many words not to do it for the speedup: the value is retiring the
duplicate work and giving the two probes the real call to ask about.

`MUTSU_VM_STATS=1` reports the change directly. The `execcallpairs` dispatch
entry gains a `carrier-preresolved` outcome, and on a 200-assertion loop under
the real `Test.rakumod` all 200 land there — previously all 200 were `carrier`,
each resolving twice.

## Pin

`tests/execcallpairs_resolves_once.rs`: `carrier-preresolved=20` with
`carrier=0` over a 20-assertion loop, a failing assertion reporting its own
line under both the native handlers and the real module, and a plain
named-argument listop call still binding its named argument.

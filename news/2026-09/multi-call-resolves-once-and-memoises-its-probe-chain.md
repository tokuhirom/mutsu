# A `multi` call resolves once, and remembers what the compiled-key probe found

Calling a `multi sub` cost 23 124 more retired instructions than calling a plain
`sub` with an identical signature — measured with callgrind on a 20 000-iteration
loop over `multi sub m(Mu $c, $d = '')` against `sub s(Mu $c, $d = '')`, both
called `(1, "x")`. That surcharge is now **13 235 Ir, a 42.8% cut**, and the whole
benchmark went from 596.3 M to 398.7 M retired instructions (-33.1%). It is the
first of the two items [#7573](https://github.com/tokuhirom/mutsu/issues/7573)
left open after #7684, which had already brought the surcharge down from 41 553.

Everything below is dispatch bookkeeping. The opcode stream is byte-identical
before and after, as it was for #7684 — reducing opcode count is not what makes a
call cheap.

## The call resolved itself twice

`dispatch_func_call_inner` asks `find_compiled_function` for a compiled body
first. For a `multi` whose winning candidate does not live in the caller's
`compiled_fns` table — the ordinary shape — that returns `None`, and the multi
branch below it then resolves the winner itself and runs it through
`compile_and_call_function_def`. Both halves called
`resolve_function_multi_cached`, so `multi_arg_type_keys` built the same cache
key twice per call: a `Symbol` per argument, plus one per argument *property*
(definedness, a `VarRef`'s declared type, an enum member).

`find_compiled_function_memo` now hands its resolution back to the caller, which
uses it instead of resolving again. The memo is only filled when the answer came
from the *type-keyed* path — `resolve_function_multi_cached_keyed` reports that
as a second return value. This matters: the un-keyed fallback runs the full
`resolve_function_with_types` candidate walk, which reads
`pending_call_arg_sources` (an `is rw` parameter accepts only a writable
lvalue), and the two call sites run under different pending sources. A candidate
set containing an `is rw` parameter is exactly what
`func_multi_dispatch_type_cacheable` refuses, so the two conditions line up and
a shared resolution is only ever reused where nothing else about the call could
have changed the answer.

## The probe chain ran in full on every call

The larger half. `find_compiled_function_inner` looks for a compiled body by
building candidate key strings and probing `compiled_fns` with each: package-
qualified, arity-qualified, type-signature-qualified, fingerprint-qualified,
walking outwards from the innermost package to `GLOBAL`. Around fifteen
`format!`ed `String`s per call. For a non-multi the answer is memoised in
`fn_resolve_cache` and the chain runs once; a `multi` is deliberately excluded
from that cache, because its key cannot tell apart two calls that share a type
signature but pick different candidates. So the chain ran in full on every
single multi call — and for the common shape all fifteen probes fail and the
answer is a constant `None`.

`multi_compiled_key_cache` memoises it, keyed by the *resolved winner's body
fingerprint* alongside everything else the chain reads: the name, the current
package and the innermost lexical package (the two inputs `bare_name_packages()`
derives its search list from), the arity and the positional arity, and the
argument type signature. The fingerprint is what makes this sound where a plain
type-signature key is not — the resolution has already picked the winner, and
the probes only ever accept a body matching it. Unlike `fn_resolve_cache` this
memo also stores the negative answer, which is the whole point. A positive hit
is still re-validated against the table and falls back to a fresh probe if it
has gone stale; the whole memo is cleared with `fn_resolve_cache` whenever
`fn_resolve_gen` moves.

While there: the type signature these keys are built from is now
`Vec<&'static str>` rather than `Vec<String>`. `value_type_name` returns a
`&'static str` to begin with, so a key that is rebuilt on every call was
heap-allocating a `String` per argument for no reason.

## What is left

`find_compiled_function_inner` is down from 225.0 M inclusive instructions
(37.7% of the benchmark) to 45.6 M, and the single remaining resolution is
40.9 M. The next-largest item on this path is `compile_and_call_function_def` at
114.9 M — 5 745 Ir per call against the 1 090 Ir an ordinary sub pays through
`call_compiled_function_positional_light`. Item 2 of #7573 (whether the
`__mutsu_test_callsite_line` marker should still be a runtime named argument)
and the flat interpretation cost of the vendored `Test` module are untouched.

`t/multi-dispatch-resolution-cache.t` pins the shapes a key-blind memo would get
wrong: one call site alternating between candidates, arity-selected candidates,
`:D`/`:U` smileys sharing a type name, a subset and a `where` constraint making
the winner depend on the value, an `is rw` candidate selected by the call site
rather than the argument type, the same bare name resolving differently in two
packages, and a redeclared body reached through `EVAL`.

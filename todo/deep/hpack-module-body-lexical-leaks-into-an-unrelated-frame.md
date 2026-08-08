# A module body's block-local `my int $i` reaches an unrelated caller's loop variable

With the multi-param `for` lane fix landed (`t/for-multi-param-shared-lane.t`),
Cro's `t/http-session-inmemory.rakutest` still fails tests 3-7 — but with a
different wrong value, and through a different mechanism. The store is no
longer involved at all: with `MUTSU_DEBUG_CLOBBER=i` instrumentation on
`SharedStore::set` / the `sync_shared_vars_to_env` apply loop, **neither fires**
for `i`. The value now arrives through the **`env` axis**.

## What is confirmed

Two independent shadow-bisect runs pin it exactly (`tmp/run-session-shadow.sh`,
which puts `tmp/shadow/lib` ahead of the vendored Cro tree and the bundled
batteries):

- Renaming the *test's* loop variable `$i` → `$zqx` makes tests 3-7 pass. So it
  is a pure name collision, not a session-logic bug — the cookie jar and the
  in-memory session store are correct.
- Renaming `$i` → `$hpi` in a shadow copy of **`modules/HTTP-HPACK/lib/HTTP/HPACK.rakumod`**
  makes tests 3-7 pass. Renaming `$i` in `Cro/HTTP/Router/LinkGenerator.rakumod`
  (the other `-1`-valued `$i` in the loaded tree) does not.

The producer is HPACK's Huffman-tree builder, a block-local native-typed lexical
inside a module-scope `constant` initializer:

```raku
my constant HUFFMAN_TREE = do {
    my int @tree = 0, 0;
    for 0..256 {
        my int $code = HUFFMAN_CODES[$_];
        my int $i = HUFFMAN_LENGTHS[$_];
        my int $tree-pos = 0;
        while --$i >= 0 { ... }      # leaves $i == -1
    }
    @tree
};
```

`-1` is exactly what the test then reports for every request
("Session cookie being sent makes state work (request -1)").

## How it reaches the victim

The final clobbering write, captured with a `Backtrace::force_capture` hook in
`Env::insert_sym` gated on the key:

```
[CLOBBER-ENV] i = Int(-1)
   1: vm_method_dispatch::merge_method_env
   2: vm_method_dispatch::call_compiled_method
   ...
  10: vm_for_loop_intrange::exec_for_loop_int_range     <-- the test's `for 1..5 -> $i`
  17: vm_control_ops::exec_block_local_scope_op
  20: vm_given_when_ops::exec_given_op                  <-- `given Cro::HTTP::Client.new(:cookie-jar) -> $client`
```

`merge_method_env` merges a callee-overlay key back into the caller whenever
`saved.contains_key_sym(k)` — i.e. "the caller has a variable of this name too".
It has no body-entry comparison (unlike `call_sub_value`, which skips a key
whose value equals the callee's body-entry snapshot), so a `-1` that reached the
`$client.get(...)` frame's overlay from somewhere deeper is written straight
over the caller's live loop variable. During one request the hook fires ~7200
times for `i`, most of them `call_sub_value` closure-env installs reached
through `check_method_wrap_chain` (the OO::Monitors wrap).

## What the loop-body block-scope fix did and did not cover

`t/loop-body-my-does-not-outlive-the-block.t` (landed separately) fixed one half
of the leak: `pop_loop_local_scope` used to restore only names that *shadowed*
an outer binding, so a body-local `my` with no enclosing namesake stayed in
`env` under its bare name forever. With that fixed, HPACK's `$i` no longer sits
in `env` — instrumenting `Env::insert_sym` on the key shows the last write is a
per-iteration declaration value (28, 30, …), never `-1`.

**The Cro symptom survives anyway.** `bash tmp/run-client-probe.sh` still prints
`after i=-1`, and after `use HTTP::HPACK` both `::('$i')` and `MY::<$i>` still
answer `-1` while `env` does not hold it. So there is at least one more channel
carrying the value; the two candidates not yet ruled out are

- `set_our_var` — the `SetGlobal` arm writes every name into the `our` store as
  well ("`::('name')` falls back to this store"), and nothing scopes that write
  to the declaring block; and
- the frame's **local slot**, which `MY::` enumerates directly (the same-file
  matrix shows `MY::<$d>` = 4 for a `for` body `my $d` even when `::('$d')` is
  correctly Nil).

Whichever it is, `merge_method_env` is still the amplifier: it writes a
callee-overlay key back into the caller whenever the caller has a variable of
the same name, with no body-entry comparison.

## Why it is deep

The obvious question — how does a block-local `my int $i` from a module body
survive into a request-time overlay at all — has no synthetic repro yet. All of
these were tried and do **not** reproduce:

- the same `my constant X = do { for … { my int $i; while --$i >= 0 {} } }`
  shape in a local module, called from a `for 1..5 -> $i` loop (with and
  without a class/method call, with and without `await start`);
- `use HTTP::HPACK` directly plus a `for 1..5 -> $i` loop calling
  `Encoder.encode-headers`.

So the carrier needs at least one more ingredient present in the Cro stack
(nested module loads, the monitor wrap chain, and per-connection threads are the
candidates). Reduction has to continue by shadow-bisecting the *Cro* layers, not
by guessing a synthetic shape.

Two plausible fix axes, neither obviously right yet:

1. **Stop the leak at the source** — a block-local `my` in a module body must
   not survive its block. Preferable, but the block scoping of native-typed
   (`my int`) declarations inside a `constant` initializer is exactly what is
   not yet understood here.
2. **Stop the propagation** — give `merge_method_env` the body-entry comparison
   `call_sub_value` already has, so a method only writes back a caller-visible
   name it actually *changed*. Cheap and general, but it treats the symptom, and
   the entry snapshot it would need is not currently threaded to that function.

## Reproducing

```
bash tmp/run-session-test.sh          # 13 tests, 3-7 fail with "request -1"
bash tmp/run-session-shadow.sh        # same, with tmp/shadow/lib first
```

Put a copy of `HTTP/HPACK.rakumod` with `$i` renamed under
`tmp/shadow/lib/HTTP/` to see 3-7 pass.

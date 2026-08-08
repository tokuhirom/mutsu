# A method wrapper only writes back the lexicals it actually mutated

`check_method_wrap_chain` propagated a wrapper's closure variables to the caller
with one rule: *if the caller has a variable of this name, overwrite it.*

```rust
if self.env().contains_key_sym(*k) {
    self.env_mut().insert_sym(*k, v.clone());
}
```

The premise — "the wrapper assigned the caller's lexical" — is false in general,
because the wrapper's captured env is not a set of free variables. Once
`reflective_name_access_possible()` latches (any `EVAL` / `::()` / pseudo-stash
use anywhere in the program does it, and Cro's dependency tree certainly does),
`capture_closure_env` snapshots the **whole env by name**. A wrapper therefore
carries every lexical that happened to be live where it was created — including
block-local leftovers from unrelated compunits — and republished all of them
over the caller's same-named variables on every wrapped call.

That rule had already been patched twice with a denylist: `$_`/`$/`/`$!` and
`self` were excluded after a monitor method reset the caller's topic and rebound
its invocant to a `MetamodelX::MonitorHOW`. The general form is a *changed*
test, not a name list: compare each persisted value against what the wrapper's
closure held before the call (its `closure_env_overrides` entry, or the `Sub`'s
own captured env on the first call) and skip the ones it did not touch. The
comparison is `cheaply_unchanged` — O(1), Arc-identity based, and conservative,
so anything it cannot classify is still written back. A container whose
*contents* the wrapper mutated stays correct either way: those are visible to
the caller through the shared cell, so skipping the handle write-back loses
nothing. The denylist stays as a belt-and-braces guard for names that are
per-frame even when they do differ.

This was the last hop of the HPACK leak. `HTTP::HPACK`'s Huffman-table builder
leaves a block-local `my int $i` at `-1`; OO::Monitors wraps every monitor
method in Cro's HTTP client, those wrappers had captured that `-1`, and each
wrapped call republished it onto the `$i` of a `for 1..5 -> $i` loop several
frames up in the user's own test file. Cro's `t/http-session-inmemory.rakutest`
went from 5/13 to 10/13 — every "Session cookie being sent makes state work"
test now passes, so the cookie jar and the in-memory session store round-trip
correctly. The three that remain are the concurrent-client pair and session
expiration, which are unrelated.

Pinned by `t/method-wrap-writeback-only-mutations.t`, which guards the risk this
change introduces: a lexical a wrapper *really* mutates must still reach the
caller.

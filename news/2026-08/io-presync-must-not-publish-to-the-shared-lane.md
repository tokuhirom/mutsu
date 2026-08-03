# A `say` no longer republishes the printing frame's locals to the cross-thread lane

Restarting an HTTP server on one port worked twice and then answered 404:

```
round 0: status=200 body='OK'
round 1: status=200 body='OK'
round 2: ERR X::Cro::HTTP::Error::Client: Server responded with 404 Not Found (GET http://localhost:31419//)
round 3: ERR X::Cro::HTTP::Error::Client: Server responded with 404 Not Found (GET http://localhost:31419///)
```

Note the target: the caller's own `my $url` had grown a `/` — once per request.
Each round asked for `"$url/"`, and each round `$url` was one slash longer than
the round before.

## Root cause

The cross-thread shared store is keyed by **bare name**. Three functions mirror a
frame's local slots into `env` in bulk so that a name-based reader can see them:

- `sync_env_from_locals` — frame teardown (`run_inner`),
- `sync_env_from_locals_declared` — run before every Say/Put/Print/Note, so a
  `$*OUT` override or a `.gist` sees fresh values,
- `sync_regex_interpolation_env_from_locals` — the same for regex interpolation.

They all go through `set_env_with_main_alias`, which does double duty: it writes
`env` *and* publishes to the shared store, marking the name dirty. For the two
incidental mirrors that is wrong twice over. The mirror walks whichever frame
happens to be printing, so it republishes that frame's `$url` — a callee's
parameter, say — into the lane that belongs to the caller's `my $url`; and the
dirty mark it leaves makes the caller's next `sync_shared_vars_to_env` pull the
lane's current content back over its own live slot. `rust-gdb` showed the chain
end to end: `SYNCPUSH "url"` at the `await`, then `CALLERWB "url"` writing it
into the caller's slot, then the corrupted value in the next round's request.

## Fix

`suppress_shared_publish` is set while those two incidental mirrors run;
`set_shared_var_sym` then writes `env` only, leaving the lane and its dirty set
untouched. Genuine cross-thread writes are unaffected — they publish from the
assignment sites, which mirror per write (`flush_local_to_env`).

Frame teardown (`sync_env_from_locals`) deliberately keeps publishing. There the
frame's own bindings, parameters included, are exactly what a worker spawned from
that routine must see by name: roast `S17-channel/stress.t`'s
`sub bogosort_concurrent(@list)` reaches `@list` from inside a `start` block
through this lane, and suppressing it made that sub read the *previous* sub's
same-named parameter instead (`1 2 3 4 5`, the earlier `sleep_sort`'s answer,
where `e l p r` was expected). That regression is what pinned down which of the
three mirrors may publish.

## Verification

- Unit pins in `src/runtime/runtime_shared_vars_tests.rs`: a suppressed write
  reaches `env` but neither updates nor dirties the lane; an ordinary write does
  both.
- Full local TAP suite (2810 files / 26678 tests) and all 99 whitelisted `S17-*`
  concurrency roast files pass, as does a 487-file `S04`/`S05`/`S16`/`S32` slice.
- Six sequential `Cro::HTTP::Server`s on one port (`tmp/mw6.p6`, `tmp/mw9.p6`)
  now serve every round; before, rounds three onward returned 404 or an empty
  body.

A self-contained Raku reproducer could not be constructed: the chain needs a
worker lineage to publish a same-named local at teardown *and* the main frame to
have marked that name dirty from a print, and every synthetic arrangement tried
was defeated by one of the existing masks (`block_captured_scalars` masks a
`start` block's captured scalars; a plain sub call heals `env` with the caller's
own value on its next print). Hence the unit pins on the mechanism itself.

## Still open

`t/http-middleware.rakutest` passes its first subtest 4/4 and then hangs, and the
other multi-server files in the vendored Cro::HTTP suite are unchanged — see
`todo/tickets/async-listener-not-freed-when-relistening-in-a-loop.md`. A separate
bare-name collision also survives: a multi-parameter `for @x.kv -> $i, $c` binds
by `Stmt::Assign` rather than declaring, so `Cro.compose`'s `$i` still leaks into
a caller's `$i` (`todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md`).

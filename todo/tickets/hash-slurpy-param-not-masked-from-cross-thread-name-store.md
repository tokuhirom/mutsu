# `*%options` slurpy receives another thread's same-named binding — `%`-sigil parameters are never masked from the cross-thread name store

## Affected tests
- `t/http-session-persistent.rakutest` subtest 16 ("Using old session for route 2"): expected `'Visit2 2'`, got `''`.

## Symptom chain (each link verified with probes)
1. `Cro::HTTP::Session::Persistent.process-responses` calls `$res.set-cookie($!cookie-name, $_, |%cookie-opts)` with `%cookie-opts = max-age => ..., :http-only, path => '/'`. A shadow probe confirms `%cookie-opts` has all 3 entries at the call site on the worker thread.
2. Inside `Cro::HTTP::Response.set-cookie($name, $value, *%options)`, a probe shows `%options` = **`{host, nodelay, port}`** — a *different* live `%options` binding from elsewhere in the Cro server/client machinery in the same process (listener options), not the flattened `%cookie-opts`.
3. Cookie built from those bogus named args → the wire header is `Set-Cookie: _session=<id>` with NO `Path=/` (verified with curl against the live mutsu server).
4. The client jar then computes the default path from the request URI: `CookieJar!default-path('/hits')` returns `'/hits'` (Cro's RFC6265-ish fallback), so the stored cookie has `path="/hits"` (verified by dumping `$client.cookie-jar.contents`).
5. `path-match('/hits', '/login')` and `path-match('/hits', '/hits2')` fail → the session cookie is only re-sent on `/hits`. `/login`'s `login = True` lands in a session the client never references again; `/hits2`'s request carries no cookie → fresh `SessionData` (login False) → `Logged` subset auth fails → 401 with empty body → `''`.

The `subset Logged of SessionData where *.login` check itself is fine in mutsu (verified standalone: `tmp/repro-subset-where-attr.raku` matches raku).

## Repro
Live-context repro: `tmp/repro-session-logged3.raku` (spins a real Cro server on port 31379, one client, prints jar contents after each request). Under mutsu the jar shows `path="/hits"` after the first response and `/login`//`/hits2` go out cookie-less; under raku the test file passes (16/16 — run the actual test with real raku to see expected).

Isolated repros that PASS (bounding the bug — the collision needs a busy multi-threaded process):
- `$res.set-cookie(..., |%cookie-opts)` single-threaded: full header (`tmp/repro-setcookie-opts.raku`).
- Same call inside `supply whenever` from a monitor method, driven cross-thread, small process: full header (`tmp/repro-setcookie-in-whenever.raku`).
- `|%hash` into a `*%options` sub/method in the same shape: correct (`tmp/repro-slip-hash-in-whenever.raku`).

So the minimal repro is still open; the live probe evidence (step 2: `%options` containing `host,nodelay,port`) pins the mechanism regardless.

## Root cause
The cross-thread shared store is keyed by BARE NAME. Scalar parameter names are masked per-call via `mask_thread_redeclared_params` (`src/runtime/runtime_shared_vars.rs:250-270`) precisely so a nested spawn / shared-store sync cannot substitute another frame's same-named binding. But that function **skips every `@`/`%`/`&` name** (`runtime_shared_vars.rs:259`: `if name.starts_with(['@', '%', '&']) { continue; }`) — aggregates deliberately "keep the name lane" for the `__mutsu_atomic_*` CAS copies (comment at `runtime_thread.rs:208-215`). Consequence: a `*%options` slurpy (or any `%`-named parameter) bound in one thread is visible/clobberable through the store by any other thread's live `%options`, and `Response.set-cookie`'s `%options` resolves to the listener's `host/nodelay/port` options hash published under the same bare name. Same architectural family as the sibling ticket `session-sibling-thread-array-name-merge.md` (name-keyed store cannot represent concurrent same-named bindings), but a distinct fix site: parameter binding, not the push lane.

## Fix direction
- Extend the parameter mask to `%`- and `@`-sigil parameter names: in `mask_thread_redeclared_params` (`src/runtime/runtime_shared_vars.rs:259`), stop skipping `@`/`%` (keep skipping `&`), masking them in `thread_param_shadow_vars` for the duration of the call the same way scalars are. The reason aggregates were excluded — the CAS lanes are keyed off base-name entries — applies to *declared lexicals*, not to per-invocation parameter bindings; a parameter's shadow must never be force-declared or synced (exactly the argument in the function's own doc comment at lines 216-249).
- Then audit the read side: `sync_shared_vars_to_env` (`runtime_shared_vars.rs:527`) and the `GetLocal` shared-preference path must respect `thread_param_shadow_vars` for `%`/`@` names too (they currently consult `thread_redeclared_vars` via `container_name_is_redeclared`, which requires the mask that line 259 refuses to set).
- If the sibling ticket's lineage-scoping of the atomic lanes lands first, re-test — it may narrow but probably not eliminate this one (the clobber here is the base-name lane, not the atomic lane).

Risks: masking `%`/`@` parameters could hide a genuinely-shared aggregate passed by name into a callback that a sibling thread mutates concurrently — but such sharing is supposed to flow through the container/cell identity, not the name lane. Watch `t/lock.t`, S17 roast files, and the Cro suite (`t/http-*` under `tmp/cro-t.sh`).

## Verification
- `tmp/repro-session-logged3.raku`: jar shows `path="/"`, all five requests carry the cookie, `/hits2` returns `Visit2 2`.
- `curl -v` against the repro server shows `Set-Cookie: _session=...; Max-Age=1800; Path=/; HttpOnly`.
- `t/http-session-persistent.rakutest` subtest 16 passes; with the other three tickets the file reaches 16/16 with a proper TAP plan (rc=0).
- Note: the file currently ALSO aborts after subtest 16 in `purge` — that is the separate `hash`-term ticket (`session-bare-hash-term.md`); both are needed for a clean run.

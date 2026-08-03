# Re-`listen`ing on one port in a loop leaks listeners and then fails to bind

```raku
my $port = 31422;
for ^4 -> $round {
    my $tap = IO::Socket::Async.listen('localhost', $port).tap(
        -> $conn { $conn.close; },
        quit => -> $e { note "round $round quit: $e" });
    my $c = await IO::Socket::Async.connect('localhost', $port);
    $c.close;
    $tap.close;
    note "round $round done";
}
```

```
round 0 done
round 1 done
round 2 done
round 3 quit: Failed to bind: Address already in use (os error 98)
round 3 done
```

raku runs all four rounds. The failure is **deterministic** (same round every run)
and predates the tap-close cascade fix — verified by A/B against `main`
(`news/2026-08/supply-tap-close-cascades-upstream.md`).

## Evidence

`ss -tan` taken while the loop runs shows **two** LISTEN sockets on the port at
once, on different addresses:

```
LISTEN  127.0.0.1:31423   0.0.0.0:*
LISTEN  [::1]:31423       [::]:*
TIME-WAIT [::1]:50528 -> [::1]:31423
...
```

`'localhost'` resolves to both `127.0.0.1` and `[::1]`, and
`std::net::TcpListener::bind` picks one; different rounds can pick different
ones, so two rounds' listeners coexist happily and a third collides with
whichever address it draws. That also explains the *wrong-answer* symptom that
shows up before the bind error: the client's `connect` resolves to the address of
a **previous** round's still-live listener, so it is served by the stale round
(`round 2 got: 'R1'`).

Only the loop shape fails. A single `listen` / `tap.close` pair frees the port
correctly (`ss` shows no LISTEN afterwards), and `set_listener_closed`
(`src/runtime/native_methods/state.rs`) does wait for the accept thread's
`stopped` acknowledgement before returning, so the close handshake itself works.

## Likely fixes to evaluate

1. **Resolve the host once and bind every address it yields** (or bind a single,
   deterministic one), so `localhost` does not silently mean "either stack" —
   raku/libuv binds what it resolved and reports a real conflict.
2. **Set `SO_REUSEADDR` before `bind`** (raku does; std's `TcpListener::bind`
   does not). Needs `socket2`, or `libc::setsockopt` behind the existing optional
   `libc` feature — note `libc` is not available in the wasm build, so this needs
   a cfg split.

Both are worth doing; (1) is the actual cause of the cross-round answers.

## Why it matters

It is the remaining blocker for the vendored Cro::HTTP multi-server tests. With
the tap-close cascade fixed, `Cro::Service.stop` really does stop the server —
so `t/http-middleware.rakutest`'s first subtest now passes 4/4 (was 2/4), but the
file then **hangs** on the later servers instead of answering from the stale
listener. `t/http-auth-basic`, `http-session-*` and `router-auth` are all the
same shape. Repro against the real dist: `tmp/mw6.p6` (six sequential
`Cro::HTTP::Server`s on one port) answers for two rounds and then returns empty
bodies.

Pin when fixed: a `t/io-socket-async-relisten-loop.t` running the loop above
(remember: never hardcode a port — listen on 0 and read the tap's `.socket-port`,
per the `t/io-socket-recv-limit.t` lesson; this repro needs a fixed port only
because it re-binds the same one deliberately, so allocate it once from a port-0
listen and reuse that number).

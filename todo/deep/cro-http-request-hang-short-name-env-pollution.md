# Cro HTTP request round-trip: the request is parsed and emitted, then never reaches the route handler

## Where the Cro campaign stands (updated 2026-08-01)

`use Cro::HTTP::Router`, `route { get -> { ... } }`, `use Cro::HTTP::Server`,
`Cro::HTTP::Server.new(:host, :port, :application($app)).start` / `.stop` all
work (#5658, #5666, #5668). The request parser now works too: with the
owner-scoped nested-type fix (`news/2026-08/nested-type-short-name-owner-scope.md`)
`Cro::HTTP::Header.parse` resolves its class-body `my grammar Header` again even
with `Cro::HTTP::Router` loaded, so the request line and every header line parse
and `Cro::HTTP::RequestParser` reaches `emit $request`.

Serving an actual HTTP request still does NOT complete: after the parser emits
the request, the route handler is never entered and
`Cro::HTTP::ResponseSerializer`'s `whenever $response-stream` never fires, so no
bytes are written and the client times out with 0 bytes received.

Repro (scratch clones under `tmp/cro-work/`, `-I` list in
`tmp/cro-work/inc-paths.txt`; clone `cro-core`, `cro-http`, `cro-tls`,
`jnthn/raku-log-timeline`, `retupmoca/P6-JSON-JWT`, `japhb/CBOR-Simple`,
`japhb/TinyFloats`, `jnthn/p6-io-socket-async-ssl` — everything else is a
bundled battery):

```
timeout 200 target/debug/mutsu $(cat tmp/cro-work/inc-paths.txt) tmp/cro-server-request.p6
```

(zsh note: expand `inc-paths.txt` inline with `$(cat ...)`; a `$VAR` expansion
is not word-split and collapses every `-I` into one argument.)

Instrumented trace (add `note` lines directly to the scratch clones):

```
server started
DBG packet 78 bytes
DBG loop expecting=RequestLine
DBG loop expecting=
DBG header-line [Host: 127.0.0.1:31415]
DBG header-line [User-Agent: curl/8.5.0]
DBG header-line [Accept: */*]
DBG emitting request (no body)
                       <- route handler's "DBG handler entered" never prints
                       <- ResponseSerializer's "DBG serializer got response" never prints
curl exit: 28 (timed out, 0 bytes received)
```

So the remaining break is in the stage between the parser's `emit $request` and
the `route` block's handler — the Cro pipeline connection (`Cro.compose` /
`Cro::ConnectionManager`) or `Cro::HTTP::Router`'s own transformer supply.

## Still open: a module-global short name beats an inner-scope enum value

Visible in the trace above as the empty `DBG loop expecting=` lines.
`Cro::HTTP::RequestParser.transformer` declares, inside its `supply` block,
`my enum Expecting <RequestLine Header Body>`, and does `$expecting = Header`.
`RequestLine` resolves to the enum value correctly, but `Header` resolves to
`Cro::HTTP::Router::Header` — the *role* — because registering that role bound
the bare short name `Header` in the ONE global env, and that binding wins over
the enum value declared in an inner lexical scope.

It happens not to break this particular request (`when Header` compares the same
wrong value on both sides, so the header branch is still taken, and a bodyless
GET never evaluates `when Body`), but it is wrong: `$expecting == Body` on a type
object and the `Body` branch can never match.

This is the residue of the original diagnosis and the part the fix above did NOT
address. Short names of package-scoped types are inserted into the ONE global env
at registration time, so a later module's type of the same short name outranks an
inner-scope declaration in an earlier one. The owner-package-chain probe
introduced by the fix covers the case where the *owner class* is asking; it
cannot cover a same-named lexical declaration (an enum value, a `my` type) in an
unrelated scope, because that declaration is not reachable from any owner chain.

The real fix is to stop registering short-name aliases in the global env across
module boundaries and keep them per-module, the way the parser's scan does — the
same architecture gap as `package_chain_var_fallback` (#5658) but for TYPE names.

## Why the two obvious guards are not enough (both tried, reverted)

- Guarding `unsuppress_name` to bare-name registrations only (so
  `Cro::HTTP::Router::Header` keeps the suppression alive) moves the failure to
  the enum case above: the suppressed-name branch's local-declaration carve-out
  (vm_var_get_ops.rs) only tolerates a non-Package env value, and by then env
  "Header" holds the Router role's Package, so the read throws
  X::Undeclared::Symbols instead. (Superseded: the landed fix keeps the
  suppression semantics untouched and adds a separate never-cleared
  `class_scoped_short_names` set instead.)
- Guarding the `"parse"|"subparse"|"parsefile"`-on-Package arm in
  `methods_dispatch_match.rs` with `has_user_method_including_role` is correct in
  itself (a class with its own `parse` method must not fall into grammar
  dispatch) but does not fix this bug — the target had already resolved to the
  wrong type by then.

## Debugging techniques that worked (reuse these)

- `tmp/cro-work` holds plain `git clone`s, not vendored sources — add
  `note "DBG: ..."` lines directly to them to bisect the pipeline stage
  (TCP accept -> conn Supply emit -> RequestParser packet -> req-line ->
  header-line -> emit request -> route handler -> ResponseSerializer).
- The raw socket path is healthy: an `IO::Socket::Async` echo server plus an
  external `curl` round-trips fine.
- Drive the client with `run 'curl', '-sS', '-i', '-m', '20', ...` from the same
  script so the server process terminates on its own (and gdb can attach).
- `rust-gdb -batch` breakpoints on the three "Unknown method value dispatch"
  sites (methods_grammar.rs / methods_instance_ops.rs /
  methods_classhow_dispatch.rs) found the guilty one immediately last time.

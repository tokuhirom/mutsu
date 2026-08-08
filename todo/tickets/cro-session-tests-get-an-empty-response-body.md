# Cro's session tests get an empty response body

`t/http-session-inmemory.rakutest` and `t/http-session-persistent.rakutest` run
but every assertion fails the same way:

```
not ok 1 - Request with no session cookie gets fresh state (1)
# expected: 'Visit 1'
#      got: ''
```

The request reaches the server and a response comes back — the status is fine —
but `await $response.body-text` is the empty string.

## What is already ruled out

A plain client/server round trip through the same stack works:

```raku
# tmp/croround.raku
my $app = route { get -> 'hits' { content 'text/plain', 'Visit 1' } }
Cro::HTTP::Server.new(:host('localhost'), :port(TEST_PORT), application => $app).start;
my $client = Cro::HTTP::Client.new;
say await (await $client.get("$url/hits")).body-text;   # "Visit 1", three times
```

So this is not the client, the server, or body-text in general. What the session
tests add:

* a `before => Cro::HTTP::Session::InMemory[SessionData].new(…)` middleware —
  a **parameterized role** instantiated as a before-middleware;
* a route whose signature takes the session object by type
  (`get -> SessionData $session, 'hits' { … }`), i.e. the auth/session
  parameter-binding path;
* `content 'text/plain', 'Visit ' ~ ++$session.count` — an rw accessor
  increment on the session object inside the route body.

Any of those failing would plausibly produce a 200 with no body: Cro's router
turns an unmatched or dying route into an empty response.

## Suggested next step

Bisect from `tmp/croround.raku` by adding one of the three features at a time.
The vendored `Cro::HTTP::Router` carries `%*ENV<CRODBG>`-gated `[DBG]` notes —
run with `CRODBG=1` first, since a route that fails to bind is exactly what those
notes report.

## History

Both files used to die on the first test with `Type check failed in assignment to
$timeout-policy` and, before that, hang. Fixed by
`news/2026-08/rw-param-does-not-hijack-a-same-named-attribute.md` and
`news/2026-08/monitor-method-no-longer-leaks-topic-and-self.md`; the empty body
is what is left.

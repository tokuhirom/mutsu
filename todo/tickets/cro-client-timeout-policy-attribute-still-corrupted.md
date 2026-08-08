# `Cro::HTTP::Client` cannot complete a request: `$!timeout-policy` is unreadable

Every `Cro::HTTP::Client` request comes back with an empty body and this error:

```
P6opaque: no such attribute '$!timeout-policy' on type Cro::HTTP::Client in a
MetamodelX::MonitorHOW when trying to get a value
  in sub !assemble-request at .../Cro/HTTP/Client/CookieJar.rakumod line 1064
  in sub request at .../Cro/HTTP/Client.rakumod line 616
  in sub get at .../Cro/HTTP/Client.rakumod line 450
```

This blocks `t/http-session-inmemory.rakutest`, `t/http-session-persistent.rakutest`,
and is the likely cause of the `rc=124` timeouts in `t/http-auth-basic*.rakutest`
and the tail of `t/http-router.rakutest`.

## Where it is

`lib/Cro/HTTP/Client.rakumod`:

```raku
has Cro::Policy::Timeout $.timeout-policy;                       # line 379
my Cro::Policy::Timeout $timeout-policy;                         # line 615, in `method request`
self!assemble-request($method, …, %options, $timeout-policy);    # line 616
method !assemble-request(…, Cro::Policy::Timeout $timeout-policy is rw, …) { … }   # line 984
    ($timeout-policy = self ?? $!timeout-policy // $default-timeout !! $default-timeout)
        without $timeout-policy;                                 # line 1064
```

## Leads

* **The reported file is wrong.** The frame says
  `!assemble-request at …/Cro/HTTP/Client/CookieJar.rakumod line 1064`, but
  `!assemble-request` is in `Client.rakumod`, and `CookieJar.rakumod` is far
  shorter than 1064 lines. So the backtrace's file and line come from different
  frames — compare `todo/tickets/callframe-line-and-file-come-from-different-frames.md`.
  Fixing that first would make this much easier to chase.
* **`MetamodelX::MonitorHOW`** in the message means the invocant's HOW is
  OO::Monitors' (`Cro::HTTP::Client::CookieJar` is a `monitor`). Either `self`
  is the wrong object at that point, or an attribute read on a monitor-HOW
  instance does not find the class's own attributes.
* `self ?? $!timeout-policy // $default-timeout !! $default-timeout` must not
  evaluate `$!timeout-policy` at all when `self` is a type object (`??`/`!!` is
  looser than `//`, so the true branch is `$!timeout-policy // $default`). Worth
  checking that mutsu short-circuits it — `Cro::HTTP::Client.get($url)` is a
  legitimate class-level call.

## History

The earlier symptom — `Type check failed in assignment to $timeout-policy;
expected Cro::Policy::Timeout but got Any` on the *second* request from one
client — was the `reconcile_attrs` bare-name scan adopting the same-named `is
rw` parameter, the caller's variable, or the caller's `my` lexical as a `:=`
attribute binding and thereby replacing the attribute. Fixed in
`news/2026-08/rw-param-does-not-hijack-a-same-named-attribute.md`; the reduced
repro now matches raku:

```raku
class P { has $.total }
class C {
    has P $.pol;
    method go() { my P $pol; self!f($pol); 'ok' }
    method !f(P $pol is rw) { my $d = P.new; ($pol = $!pol // $d) without $pol }
}
my $o = C.new; say $o.go; say $o.go;
```

After that fix the test file gets *further* — it now runs both of its tests
instead of dying on the first — but neither passes.

## How to reproduce

```
bash tmp/cro-t.sh t/http-session-inmemory.rakutest
```

(helper scripts and the vendored Cro checkout live under `tmp/`, which is
gitignored; re-fetch with the Cro campaign's `inc-paths.txt` recipe if the
checkout is gone). Copy `Client.rakumod` into a shadow tree that `-I` puts first
to instrument line 1064 — the `%*ENV<CRODBG>`-gated notes already in the
vendored file do not cover it.

# `Cro::HTTP::Client`'s `$!timeout-policy` still turns into `Any` after one request

Every `Cro::HTTP::Client` test that makes two requests from the same client dies
on the second one:

```
Type check failed in assignment to $timeout-policy; expected Cro::Policy::Timeout but got Any
  in sub !assemble-request at .../Cro/HTTP/Client.rakumod line 1064
  in sub request at .../Cro/HTTP/Client.rakumod line 616
  in sub get at .../Cro/HTTP/Client.rakumod line 450
```

This blocks `t/http-session-inmemory.rakutest`, `t/http-session-persistent.rakutest`,
and is the likely cause of the `rc=124` timeouts in `t/http-auth-basic*.rakutest`
and the tail of `t/http-router.rakutest`.

## What is known

The relevant declarations (`lib/Cro/HTTP/Client.rakumod`):

```raku
has Cro::Policy::Timeout $.timeout-policy;                       # line 379
my Cro::Policy::Timeout $timeout-policy;                         # line 615, in `method request`
self!assemble-request($method, …, %options, $timeout-policy);    # line 616
method !assemble-request(…, Cro::Policy::Timeout $timeout-policy is rw, …) { … }   # line 984
    ($timeout-policy = self ?? $!timeout-policy // $default-timeout !! $default-timeout)
        without $timeout-policy;                                 # line 1064
```

Instrumenting line 1064 (copy the file into a shadow tree that `-I` puts first)
shows, on one client instance:

* **first request** — `$!timeout-policy.^name` = `Cro::Policy::Timeout`,
  `.defined` = `False`; the right-hand side evaluates to
  `Cro::HTTP::Client::Policy::Timeout`. Correct.
* **second request** — `$!timeout-policy.^name` = **`Any`**, `.defined` =
  **`True`**. The attribute has been replaced between the two calls.

Because it is now *defined*, `without $timeout-policy` … actually still holds
(the local is undefined), the assignment runs, and the failure surfaces as a type
check on the rw parameter's writeback into the caller's
`my Cro::Policy::Timeout $timeout-policy`.

## Not the same bug as #6061

`news/2026-08/rw-param-does-not-hijack-a-same-named-attribute.md` fixed the
`reconcile_attrs` bare-name scan adopting a same-named `is rw` parameter (or a
caller variable) as a `:=` attribute binding. That is the same *shape* — the
parameter and the attribute are both called `timeout-policy` — and it fixed the
reduced repro:

```raku
class P { has $.total }
class A {
    has P $.pol = P.new(total => 7);
    method run() { my P $q; self!fill($q) }
    method !fill(P $pol is rw) { }
}
A.new.run;   # attribute used to be replaced by the parameter's cell
```

but the Cro symptom survives it unchanged, so a second path corrupts the
attribute. Candidates not yet checked:

* `method request` / `method get` are `multi method`s, so dispatch may go
  through the interpreter slow path (`runtime/resolution_call_sub.rs`, which has
  its own `changed_caller_locals` handling) rather than
  `vm/vm_method_dispatch.rs`'s `call_compiled_method{,_fast}` where the fixed
  scan lives. The backtrace frames read `in sub !assemble-request` / `in sub
  request`, which is consistent with that.
* `Cro::Policy::Timeout` is a **parameterized role** (`role
  Cro::Policy::Timeout[%phase-defaults]`) used bare as a type constraint. The
  attribute's declared-type machinery may not resolve it the same way on the
  second pass.

## How to reproduce

```
bash tmp/cro-t.sh t/http-session-inmemory.rakutest
```

(helper scripts and the vendored Cro checkout live under `tmp/`, which is
gitignored; re-fetch with the Cro campaign's `inc-paths.txt` recipe if the
checkout is gone). The `%*ENV<CRODBG>`-gated notes already in the vendored
`Client.rakumod` do not cover line 1064 — add one there in a shadow copy.

# An `is rw` parameter no longer hijacks a same-named attribute

An `is rw` / `is raw` scalar parameter binds through a shared `ContainerRef`
cell. The method-exit attribute reconcile (`reconcile_attrs`) scans each
attribute's bare name for such a cell, to recover a `:=` attribute binding
(`$!x := $outer`) whose authoritative value lives in env/locals rather than in
the instance cell. It found the *parameter* and adopted it as the attribute's
new value — permanently replacing the attribute:

```raku
class P { has $.total }
class A {
    has P $.pol = P.new(total => 7);
    method run() { my P $q; self!fill($q) }
    method !fill(P $pol is rw) { }     # <-- same name as the attribute
}
my $a = A.new;
$a.run;
say $a.pol.total;    # raku: 7   mutsu: "No such method 'total' for invocant of type 'Package'"
```

There were two ways in:

* the frame's **own parameter** sharing the attribute's bare name;
* a **caller's variable** of that name, reachable because a callee's env is the
  flattened caller env — the very hazard the `frame_has_container_ref` gate
  above the scan documents ("a caller-frame ContainerRef … is NOT a binding made
  by this method") but could not catch, since the flattened copy lands in the
  overlay it inspects.

## Fix

The bare candidate is the only dangerous one — no lexical can be called `!x` or
`@.x` — so it is now honoured only for a name **this frame itself owns as a
slot** (which is how a sigilless `has $x` is seeded) and that is not one of the
method's parameters. The twigil candidates keep their env fallback unchanged.

Pinned by `t/rw-param-does-not-hijack-same-named-attribute.t`, which also covers
the two behaviours that must survive: a genuine `:=` attribute binding still
tracks its target, and an rw parameter still writes back to its caller.

## Where it was found

`Cro::HTTP::Client` declares `has Cro::Policy::Timeout $.timeout-policy` and a
private `method !assemble-request(… Cro::Policy::Timeout $timeout-policy is rw …)`.
Its first request left the attribute holding the parameter's cell, so the second
died with `Type check failed in assignment to $timeout-policy; expected
Cro::Policy::Timeout but got Any`.

That client-side failure is **not fully resolved** by this fix — see
`todo/tickets/cro-client-timeout-policy-attribute-still-corrupted.md`.

# A `:$param` (named) with a user-declared `subset` type is not type-checked at binding

Discovered while re-measuring `t/http-router.rakutest` (vendored Cro::HTTP
suite) after fixing `cro-router-slurpy-where-clause-nonmatch-hangs.md`: once
the router no longer hangs, tests 191/192/194/195 ("Non-matching (optional)
unpack gives 400 error (subset, Str/Int)") fail — mutsu returns 404 where
Cro (and raku) return 400.

Root cause isolated to a general (Cro-independent) binding bug, not a router
bug. Minimal repro:

```raku
subset UUIDv4 of Str where /^ <[0..9a..f]> ** 8 $/;
sub f(UUIDv4 :$id!) { "ok $id" }
say f(id => "lol");
```

- raku: throws `Constraint type check failed in binding to parameter '$id';
  expected UUIDv4 but got Str ("lol")`.
- mutsu: prints `ok lol` — the subset's `where` constraint is silently
  skipped for a **named** parameter.

For comparison, the same subset on a **positional** parameter is enforced
correctly (fixed by PR #6277, `parameter-type-not-nominalized-for-user-
subsets.md`) — so this is specifically the named-parameter binding path not
sharing that check. `Signature.ACCEPTS` also gets this right for named
params in isolation (see `t/signature-accepts-slurpy-where.t`'s sibling
concerns) — the gap is in real call binding for `:$name` params, not in
`ACCEPTS`.

Likely fix location: wherever named-parameter binding does its type/where
check (`src/runtime/types/binding_signature.rs`, the named-param branch —
compare with the positional branch that PR #6277 already fixed to nominalize
user subsets via `registry().subsets`).

To reproduce via Cro: `bash tmp/cro-t.sh t/http-router.rakutest` (see
`handoff-cro-next-steps` session memory for the Cro campaign scaffolding
under `tmp/cro-work/`), tests 191/192/194/195.

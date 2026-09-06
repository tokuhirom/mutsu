# A closure's captured `@`/`%` lexical loses to a same-named array in the calling frame

Measured 2026-09-06 alongside ADR-0055's scalar fix
(`news/2026-09/adr0055-unvouched-escaping-captures-get-a-cell.md`). This is the
`@`/`%` half of the same family and is INDEPENDENT of it: it reproduces with no
scalar anywhere in the program, and none of the scalar cell/vouch machinery
touches it (`box_captured_lexicals` skips every `@`/`%`/`&` name by design).

## Repro

```raku
my @a = 1, 2;
@a.push(3);
my $f = -> { @a.elems };
sub collide() { my @a = 9; my $g = { @a.elems }; $g.(); $f.() }
say collide();          # raku: 3    mutsu: 1
```

`@a.push(3)` is load-bearing: it is an in-place container write, which
`own_container_writes` refuses to vouch for, so `@a` is not in
`authoritative_free_vars`. `my $g = { @a.elems }` is load-bearing in the same way
as in ADR-0055 §1.2(b): it forces the caller's `@a` into `env` where the merge's
don't-overwrite probe can find it. Remove either line and mutsu answers `3`.

## Why this is its own ticket

ADR-0039 ("container lexicals resolve lexically") is the decision this violates,
and its mechanism — `box_decl_local_container_cell` / the `@`/`%` decl-site
boxing — is a different code path from the scalar `box_captured_lexicals`
trigger ADR-0055 extended. The scalar fix (`needs_cell_unvouched_locals`) cannot
be pointed at it: the merge's force-overwrite exception keys on
`ValueView::ContainerRef`, and an `@`-sigil capture reaches the merge as a plain
`Array` value that is already reference-shared, so "overwrite vs don't" decides
*which array object* the name resolves to, not whether a mutation is visible.

The likely shape of the fix is the same as the scalar one — make the vouch's
complement within the escaping-captured set an independent trigger for the
container-cell path too — but the acceptance surface is entirely different and
the `Array`/`Hash` merge branches (`resolution_call_sub.rs` has an explicit
"both sides are Arrays, skip" arm) have to be re-derived first.

## Acceptance

The repro prints `3`; a `t/` pin covers `@` and `%`, both in the hijack
direction and in the staleness direction (a post-capture `push` must be visible
to the closure); full roast delegated to CI.

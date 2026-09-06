# A method hands back the container it was given — the `.snitch` finding, closed

`my $a = 42; $a.snitch = 5` prints `42` and then leaves `$a` holding `5` in
raku. mutsu threw `X::Assignment::RO`. That finding opened as
`todo/deep/native-method-cannot-return-an-lvalue-container.md`, was designed as
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md),
and is closed now that the ADR's last slice (the subscript-receiver producer)
has landed and the ADR is Accepted.

## Every row of the finding, re-measured

Verified 2026-09-06 against raku v2026.07 and a debug `mutsu` on `main` —
byte-identical in both:

| # | Program | raku / mutsu |
|---|---|---|
| A1 | `my $a = 42; $a.snitch(&say) = 5; say $a` | `42` then `5` |
| A2 | `augment class Any { method mysn(\SELF:) is raw { SELF } }; $b.mysn = 5` | `5` |
| A3 | `method nonraw(Any:D $s:) is raw { $s }` — non-raw invocant | refused, `42` |
| A4 | `method notrw(\S:) { S }` — raw invocant, not rw-capable | refused, `42` |
| A5 | `class C1 { method m(\x) is raw { x } }; C1.new.m($a) = 5` | `5` |
| A6 | `class C2 { method m(\x) { return-rw x } }; C2.new.m($b) = 6` | `6` |
| A7 | `class C3 { method m(\x) is rw { x } }; C3.new.m($c) = 7` | `7` |
| A8 | `sub f(\x) { x = 7 }; f($d)` / `f(@e[0])` | `7` / `[7 2]` |
| A9 | `class D { method mut(\S:) { S = 7 } }; $g.mut; say $g` | `7` |

A3/A4 are the contract controls the finding named: raku needs the invocant
parameter raw **and** — for the *outbound* direction — the routine rw-capable,
and dropping either must keep refusing. They still refuse.

## Which slice closed which claim

The finding recorded two gaps and two corrections. All four are settled:

- **"The invocant never arrives as a container"** (A9) — slice 3b, which boxes
  the receiver's storage location and arms a one-slot channel the parameter
  binder consumes. `news/2026-09/…` and ADR-0067's slice 3b section.
- **"The method lvalue gate tests `method_def.is_rw` alone, and `MethodDef` has
  no `is_raw` field"** (A5/A6) — slice 2, which gave methods the same
  rw-capability oracle the sub path uses (`is rw || is raw || return-rw`).
- **"`.VAR` is not an acceptance case"** and **"this is not a native-method
  problem"** — both corrections the finding itself had already made, and
  ADR-0067 carries them forward in its Corrections 1 and 2 rather than
  re-deriving them.
- **"Adding `snitch` to the `.item` compile-time erasure is not the fix"** — the
  finding was right, and the ADR records why in "What `.item` is, and why it is
  not the design": erasure is sound only for a *pure* method, and `.snitch`
  notes its invocant.

The rows this file listed as out of reach at the time — a subscript receiver
(`@a[0].mut`), an attribute-accessor receiver (`$d.v.mut`), an argument-position
accessor — were each closed by a later slice of the same ADR; see
`news/2026-09/subscript-receiver-raw-invocant-producer.md` and
`news/2026-09/rw-result-container-consumers.md`.

## What is still open, deliberately

The readonly-*enforcement* half is not part of this contract and stays open: a
raw-invocant body that writes through an invocant it was given no location for
(an rvalue `42.mut`, an immutable `List` element) is refused by raku and
silently does nothing in mutsu. That is ADR-0067's L4/L5/M1/M2 family, tracked
in `todo/tickets/immutable-list-element-write-is-silently-dropped.md` and the
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` survey. The
*observable* half already matches — the caller's data is not modified — so these
are missing diagnostics, not wrong answers.

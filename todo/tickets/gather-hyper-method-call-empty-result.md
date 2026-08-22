# Hyper method call (`».method`) / `.map` on a `gather {...}` Seq returns empty instead of forcing it

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:312` — the `indir` routine's own doc example).

## Repro

```raku
say indir("/tmp", {
    gather { take ".".IO }
})».CWD;
```

- `raku`: `(/home/.../cwd-before-indir)` — one element, the CWD of the process before
  `indir` temporarily changed it.
- `mutsu` (`target/debug/mutsu`): `()` — empty.

## Minimal isolation (no `indir` needed)

```raku
my $g = gather { take ".".IO };
say $g».CWD;   # raku: (cwd); mutsu: ()
say $g.map(*.CWD);  # raku: (cwd); mutsu: (...) -- literally the 3-dot placeholder string
```

So this isn't specific to `indir` at all — a `gather`-produced lazy `Seq`, when
`.map`'d or hyper-method-called directly (without first being reified some other way,
e.g. by assigning to an `@`-sigiled variable or calling `.elems`), silently produces no
elements. `.map` is even worse: it doesn't error, it returns the literal 3-dot
placeholder gist string wrapped as if it were a value, rather than either the mapped
result or a genuine still-lazy Seq.

## Root cause hypothesis (needs verification, not fixed here)

Likely related to — but not explicitly listed as a residue of — the already-Deferred
"Lazy-list cluster" in `docs/doc-diff-backlog.md` (gather/take reification,
`vm_for_loop_dispatch.rs`'s `__mutsu_array_storage` guard, etc.). This ticket is filed
separately because the specific symptom (a *directly* mapped/hyper-called gather Seq,
with no intervening variable) isn't among that cluster's explicitly enumerated
remaining items — worth checking whether a future fix for one also fixes the other,
or whether this is a distinct gap in how `.map`/`».` dispatch onto a not-yet-reified
`LazyList` value.

## Affected files (starting point)

- `src/vm/vm_call_ops.rs` (hyper method call `».` dispatch) and wherever `.map` is
  implemented for a `ValueView::LazyList` receiver — needs to force/iterate the
  gather's lazy Seq rather than treating it as already-empty or rendering its
  placeholder gist as if it were the mapped value.

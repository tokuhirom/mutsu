# A package-qualified call to a module's `proto` fails; blocks a `gather` forced from consumer scope

Split out of `todo/tickets/digest-dist-blockers.md` (2026-08-17) — found while bundling the
`Digest` dist ([docs/batteries/digest.md](../../docs/batteries/digest.md)) while fixing
`Digest::SHA3`; not itself a `Digest` blocker (the dist's own `t/` suite passes in full via the
exported `sha3_256` entry point, which does not take this path), a general interpreter gap. **Not
independently re-verified with a fresh repro on 2026-08-17** — a quick attempt at a minimal
`unit module` + `proto sub ... is export` + package-qualified call from an importing script did
NOT reproduce a mismatch (raku itself rejected that shape as an unexported symbol, differently from
either bullet below). Whoever picks this up should re-derive a precise, currently-reproducing
repro first (the original investigation used `Digest::SHA3::Keccak`, a private, non-exported
`multi`/`proto` inside `Digest::SHA3`, which is not the same shape as a plain `is export`d proto —
try that shape, or dig up the exact `Digest::SHA3.rakumod` internals that produced the original
symptom) before starting design work — see the "trap-todo-deep-files-go-stale" lesson: repros in
stale tickets can be silently invalidated by unrelated fixes.

## Two linked symptoms (as originally reported, 2026-08-xx)

1. **Package-qualified call to a module's `proto` from outside the module.** A call of the shape
   `T::K8::Keccak(...)` (package-qualified, from a script that has `use`d the module) reports `No
   matching candidates for proto sub: T::K8::Keccak`, even though the identical *unqualified* call
   made from *inside* the module resolves correctly. Reproduced originally with a two-candidate
   `proto`, no `gather` involved at all — so this is the more fundamental of the two symptoms.

2. **A `gather` created inside a module routine, forced from the *consumer's* top-level scope,
   cannot resolve a module-private name.** `Digest::SHA3::Keccak(...)` called directly from a
   script (rather than through the module's exported `sha3_256` wrapper) died with `Unknown
   function: Keccak` when its `samewith` fired during lazy forcing. Going through the exported
   `sha3_256` works, because the force then happens with the module's own scope already in view —
   i.e. *lazily forcing a gather outside the frame that created it does not carry that frame's
   package-resolution context with it.* The `samewith` context capture records only the routine
   *name*; making it also carry the declaring *package* needs symptom 1's package-qualified proto
   dispatch to work first, since that is the mechanism that would resolve the recorded name back to
   the right routine.

## Affected files

- Whatever compiles/executes a package-qualified call (`Pkg::Name(...)`) to a `proto` sub —
  compare its candidate-resolution path against the unqualified in-module call, which already
  works, to find where the package qualifier gets lost or mishandled for `proto`/multi dispatch
  specifically (plain non-multi package-qualified calls are unaffected).
- The lazy `gather`/`take` forcing machinery and its `samewith` redispatch context (search for
  where `samewith`'s target routine name is captured/recorded at `gather` creation time) for
  symptom 2, once symptom 1 is fixed.

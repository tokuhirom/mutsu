# The site grows an Internals section, with generated opcode and type references

There was no human-readable list of the VM's instructions anywhere. The only
full inventory was `enum OpCode` itself: about 370 variants across 11,000
lines of `src/opcode.rs`, with their costs in a different file
(`src/vm/vm_exec_dispatch.rs`). `docs/opcode-design-review.md` is an audit of
the instruction set, not a reference to it, and CLAUDE.md's own opcode count
had sat stale at "~100" for months while the set grew past 340. The value
representation and the collector were in the same state: well documented
across ADRs, but described nowhere as a whole.

The site now has an **Internals** section (`site/internals.html`, in the nav):

- **The hub** is prose in both languages. It covers the pipeline (parser →
  compiler → `CompiledCode` → VM, plus the JIT and TRIR tiers), the 8-byte
  NaN-boxed `Value` (mutsu's counterpart to Perl 5's SV), the stack VM and its
  two JIT tiers, and the Bacon–Rajan cycle collector: candidates, trial
  deletion, the size-based trigger, safepoints and the cooperative
  stop-the-world.
- **`opcodes.html`** lists every opcode by family, with its operands, its
  `///` doc comment, the `// Cost:` line from its dispatch arm, and links to
  both source locations. It can be filtered, and the filter is kept in the URL
  (`#q=`).
- **`types.html`** lists every `Kind` a `Value` word can carry, grouped by
  where its payload lives (inline, `Arc`, cycle-collected `Gc`, `WeakGc`).
  The grouping is taken from `payload_op`, the match that actually bumps and
  releases each payload, not from the enum's comments. That matters because
  the comments file `Pair`/`ValuePair` under "Arc-backed" when the code holds
  them as `Gc`. The page also shows the built-in type tree built from the
  Rakudo-captured MROs in `builtin_type_catalog.rs`. A class with several
  parents sits under its first parent and names the rest.

The lists are generated, never written by hand.
`scripts/gen-internals-manifest.py` reads them out of the source at deploy
time (`pages.yml`) and before the site e2e test (ci.yml). The two JSON files
are git-ignored, so the published pages always describe the commit they were
built from. The script refuses to publish rather than emit a truncated
listing: this covers an enum whose layout it no longer recognises, a
`Kind` with no `payload_op` arm, and a catalog row it cannot parse.

Building the page turned up one arm with no `Cost:` line,
`TagElementSourcePath`, which now has one. It also showed that 180 of the
374 opcodes have no `///` doc comment. The page marks 176 of them
"undocumented"; the other 4 carry only a plain `//` note, which is shown as a
developer note. Documenting an opcode now means adding the comment in
`src/opcode.rs`, and the page picks it up on the next deploy.

Adding a tenth nav entry pushed the language switch onto a second row at
1280 px in Japanese. The nav's link padding and gaps were tightened slightly
so it fits on one row again.

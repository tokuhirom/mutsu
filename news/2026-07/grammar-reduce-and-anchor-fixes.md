# Five grammar/regex correctness fixes surfaced by the YAMLish battery

Driving the `YAMLish` battery (`docs/batteries/yaml.md`) past its grammar-parse
blocker uncovered five independent, general correctness bugs in the regex/grammar
engine. All are fixed here, each with a focused pin.

- **`$<name>` now reads the same `$/` as `$/<name>`.** The named-capture twigil
  read `env["/"]` directly instead of the `$/` variable's dual-store slot, so a
  nested regex operation inside a grammar action (a `.subst`, an `m//`, a `~~`)
  — which rebinds the dynamic `$/` to its own, possibly failed, match — made
  `$<foo>` read as `Any` while `$/<foo>` still saw the intact `method act($/)`
  parameter. `exec_get_capture_var_op` now resolves `$/` slot-first.
  (`t/capture-var-topic-slot.t`)

- **Proto-regex `:<name>` shorthand variants register under the proto.** A
  candidate spelled `token element:<int> {...}` (the bare shorthand for
  `:sym<int>`, differing only in that it does not bind a `<sym>` literal) was
  dropped by every resolver path, which only matched `:sym<`. `<element>` then
  fell through to a "No such method" error. (`t/proto-token-bare-variant.t`)

- **`<|w>` word boundary is implemented.** It previously matched nothing; it now
  lowers to the word-boundary assertion. (`t/proto-token-bare-variant.t`)

- **A parent rule's `{ … }` action sees child captures via `$/.hash`/`$/.values`.**
  The reduce-time `$/` was built with an empty capture set, so
  `{ make $/.values[0].ast }` produced Nil even though `$<child>.made` worked.
  The reduced, ast-carrying child matches are now folded into `$/`'s
  `named`/`list`. (`t/regex-reduce-values-ast.t`)

- **A `$` end-anchor may be followed by more atoms.** Only `$$` and a *trailing*
  `$` were treated as anchors; a bare mid-pattern `$` fell through to a literal
  `$` in Match mode, so `token plain { ^ .* $ { make … } }` demanded a literal
  `$` in the input and never matched. (`t/regex-end-anchor-then-atom.t`)

With these, `use YAMLish; load-yaml("42")` parses and concretizes simple scalars
in isolation. One battery blocker remains (a `Schema::Core` element `.ast` lost
inside the full module) — tracked in
`todo/deep/yamlish-absent-capture-any-not-nil.md`.

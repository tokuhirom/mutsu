# `:env`/`:ENV` accepts a List, not only a literal Hash

`run(..., :env(%*ENV, |%extra))`, `shell`, and `Proc::Async.start`/`.new`'s
`:ENV` used to silently drop the extra pairs of a List-valued `:env`/`:ENV`.
`:env(%*ENV, |{XX => "1"})` parses as a `List` — a `Hash` followed by a
`Slip` of `Pair`s — and Rakudo coerces it with hash semantics (`.hash`)
before building the child environment, layering the extra pairs over
`%*ENV`. mutsu's `extract_proc_options` (shared by `run`/`shell`) and
`Proc::Async`'s own `:ENV` handling both matched only `ValueView::Hash`
directly, so a List value matched neither arm and the whole option was
silently ignored beyond marking it "explicit".

The idiom is common in test files that re-run a fixture under a modified
environment — `t/vm/codegen/adr0110-trir-differential.t`'s `MUTSU_TRIR =>
'off'` run used to hit exactly this gap, comparing TRIR-on against TRIR-on
and never able to fail (worked around with a pre-built hash before this fix
landed).

Both call sites now coerce with the same `coerce_to_hash` helper the rest of
the interpreter already uses for `%h = %a, |%b`-shaped merges, so a List,
a Seq, or any other `.hash`-coercible value works the same way a literal
Hash always did.

See [issue #9085](https://github.com/tokuhirom/mutsu/issues/9085).

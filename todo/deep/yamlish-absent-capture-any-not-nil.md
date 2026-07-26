# YAMLish battery: remaining `Schema` reduce blocker

Blocker chain for the `YAMLish` battery (`zef:leont`, safe-by-default YAML,
459 dependents — see `docs/batteries/yaml.md`). With the grammar-parse /
`nextwith` fix (PR #5447) landed, `use YAMLish; load-yaml("42")` now *parses*,
and this ticket has since driven **five** general grammar/regex correctness
fixes (below). One blocker remains before the battery round-trips.

## Fixed on the way here (all general, not YAMLish-specific)

1. **`$<name>` read the wrong `$/`.** `$<name>` is sugar for `$/<name>` but it
   read `env["/"]` directly instead of the `$/` variable's dual-store slot. A
   nested regex op (`.subst`, `~~`) inside a grammar action writes `env["/"]` to
   its own — possibly failed — match, so `$<properties>` became `Any` while
   `$/<properties>` still saw the intact `method plain($/)` parameter. This was
   the original "absent capture is Any not Nil" symptom. Fix:
   `exec_get_capture_var_op` now reads `$/` via `locals_get_by_name` (slot first,
   env fallback). Pin: `t/capture-var-topic-slot.t`.
2. **Proto-regex `:<int>` shorthand variants were dropped.** The candidate
   resolver only matched `:sym<`, so `token element:<int> {...}` never registered
   under `proto token element`. Fix: `is_proto_variant_suffix` /
   `extract_variant_ident` recognize both `:sym<int>` and the bare `:<int>` /
   `:«int»` forms across all resolver paths. Pin: `t/proto-token-bare-variant.t`.
3. **`<|w>` word boundary was unimplemented** (matched nothing). Now lowers to
   `RegexAtom::WordBoundary`. Pin: `t/proto-token-bare-variant.t`.
4. **Reduce-time `$/.hash` / `$/.values` were empty.** A parent rule's trailing
   `{ … }` action saw an empty `$/` capture set, so `{ make $/.values[0].ast }`
   produced Nil (only `$<name>` carried children's `.made`). Fix:
   `setup_regex_code_block_env` + `reduce_regex_captures_made_for_rule` now fold
   the ast-carrying child matches into `$/`'s `named`/`list`. Pin:
   `t/regex-reduce-values-ast.t`.
5. **A `$` end-anchor followed by any atom fell through to a literal `$`** in
   Match mode (`$$` and a trailing `$` were handled; a mid-pattern `$` was not),
   so `token plain { ^ .* $ { make … } }` never matched. Fix: the bare-`$` anchor
   is recognized in Match mode too. Pin: `t/regex-end-anchor-then-atom.t`.

## Remaining blocker: `Schema::Core` element `.ast` lost in the full module

In isolation the schema now works:

```
Schema::Core.new.parse("42").ast   # => 42   (extracted grammars)
```

but inside the full `lib/YAMLish.rakumod` the same call yields `(Nil,)`:

```sh
mutsu -I <yamlish>/lib -I modules/MIME-Base64/lib -e 'use YAMLish; say load-yaml("42").raku'
# => Any   (raku: 42)
```

`Single::make-value` sees `$schema.new.parse($!value)` match with
`.ast == (Nil,)` — a 1-element list of Nil — instead of the scalar `42` the
element token's `{ make $/.Str.Int }` should have produced. So a) the child
element's `.made` is not reaching the `Schema::JSON` TOP's
`{ make $/.values[0].ast }`, and b) the result is wrapped in a list.

### What is and isn't reproduced

- `tmp/schema_ex.raku` (the extracted `Schema::JSON` + `Schema::Core` grammars,
  lines 784-883) → **42** (correct).
- `tmp/gram_ex.raku` (the main `grammar Grammar`, lines 150-782, PLUS the schema
  grammars) → **`(Nil,)`**. So something in the ~780-line main `Grammar` — which
  shares many token names with the schemas (`element`, `plain`, `ws`, `space`,
  `newline`, …) and is itself named `Grammar` (shadowing the built-in that the
  schema grammars implicitly inherit) — perturbs the schema reduce.
- A minimal main `Grammar` (just a list-making `TOP` + `element`/`plain`/`ws`),
  a same-named `grammar Grammar` parent, and a `token element(Str,Int)` collision
  all reproduce **42** individually (`tmp/collide*.raku`, `tmp/bis1.raku`), so the
  trigger is a *specific* interaction not yet isolated.

### Narrowed: it is the main grammar being *named* `Grammar`

Renaming the main `grammar Grammar` → `grammar MainG` (leaving everything else
identical) flips `Schema::Core.new.parse("42").ast` from `(Nil,)` back to `42`
(`tmp/gram_ren.raku`). So the trigger is that the schema grammars implicitly
inherit the **module-local** `grammar Grammar` (which blocker #2 made shadow the
built-in `Grammar` for type resolution) instead of the core `Grammar` — mutsu's
`Schema::JSON.^mro` is `(Schema::JSON, Grammar, Any, Mu, Match, Capture, Cool)`,
and that `Grammar` is the 780-line user grammar, not the Cursor.

In Raku a grammar with no explicit `is` parent always inherits the **core**
`Grammar`; a user grammar that merely happens to be named `Grammar` must NOT
become the default parent of other grammars. mutsu now resolves the implicit
parent through the shadow.

But the minimal versions still return `42`: a schema inheriting a small
`grammar Grammar` — even one whose inherited `element(Str,Int)`/`ws`/`space`
tokens collide with the schema's own — parses correctly (`tmp/inh3.raku`,
`tmp/collide3.raku`). So it is the *specific* content of the full 780-line main
grammar, inherited via the wrong parent, that breaks the schema reduce (and wraps
the scalar in a 1-list). Two candidate root causes, to be separated next:

1. **Wrong implicit parent** (the real bug): the schema should inherit core
   `Grammar`, not the module's. Fix where a grammar's implicit parent is
   resolved so it targets the core Grammar, without regressing blocker #2's
   type-resolution shadow (`t/grammar-named-grammar-in-module.t`). This likely
   fixes #3c outright, since with the correct parent the schema stops inheriting
   the 780 tokens entirely.
2. If the parent must stay resolvable to the user grammar, bisect which inherited
   token perturbs the reduce; the `(Nil,)` list-wrapper (vs the main TOP's
   `make (@<document>».ast)`) is the distinctive clue.

Also seen: mutsu orders the grammar MRO `… Any, Mu, Match, Capture, Cool` where
raku is `… Match, Capture, Cool, Any, Mu` — a separate MRO-ordering bug, not yet
shown to matter here.

### Next step

Start with candidate (1): find the grammar implicit-parent resolution and make an
unqualified grammar inherit the **core** `Grammar`, not a same-named user
grammar. Verify `Schema::Core.new.parse("42").ast == 42` in `tmp/gram_ex.raku`
and that `t/grammar-named-grammar-in-module.t` still passes.

## Repro setup

Module tarball: `~/.cache/mutsu-yaml-survey/yamlish/YAMLish-0.1.3`. Emitter is
`save-yaml`/`save-yamls`. Run with `-I <lib> -I modules/MIME-Base64/lib`. A
throwaway instrumented copy lives at `tmp/ybis/YAMLish.rakumod`.

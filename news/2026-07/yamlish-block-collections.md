# YAMLish block collections parse identically to raku

`load-yaml` round-trips block sequences (`- 1\n- 2`) and block mappings
(`a: 1\nb: 2`) exactly as raku does. This entry keeps the full blocker chain that
got there, because it is the clearest record of how mutsu's regex engine handles
an indentation-driven grammar. The remaining packaging work — vendoring the
module, the release gate, the docs — landed alongside
`yamlish-upstream-suite-passes.md`.

The regex half landed as PR #5510; the three downstream gaps are listed at the
bottom.

Blocker chain for the **YAMLish** battery (`zef:leont`, safe-by-default YAML, 459
dependents — `docs/batteries/yaml.md`). The **scalar path now works**:
`use YAMLish; load-yaml("42")` yields `42`, `"hello"`, `True`, `3.14`, etc. Two
general fixes landed getting here (see below). What remains is **block
collections** — sequences (`- 1\n- 2`) and maps (`a: 1\nb: 2`) — all of which
route through YAMLish's *indentation-tracking* grammar machinery and hit two
missing regex-engine features.

## Landed on the way here (both general, both merged)

1. **Sibling grammar inherits the core `Grammar`, not a module-local
   `grammar Grammar`.** `qualify_sibling_parent_name` was rewriting the implicit
   `Grammar` default parent to `YAMLish::Grammar`, so `Schema::JSON` /
   `Schema::Core` inherited the 780-line main grammar's tokens/Actions/`parse`
   override and reduced with the wrong Actions (scalar came back `(Nil,)` /
   `Any`). Fix: exclude `Grammar` from `qualify_sibling_parent_name`. PR #5458,
   pin `t/grammar-sibling-implicit-core-parent.t` (+ `t/lib/GrammarSiblingCore.rakumod`).
2. **General `<?subrule>` / `<!subrule>` zero-width lookahead assertions.** Only
   the special forms (`<?before>`, `<?[...]>`, `<?alpha>`, `<?wb>`, `<?same>`,
   `<?:Prop>`, `<?@var>`) were handled; a general `<?userToken>` fell through to a
   literal string match. YAMLish's `list-entry` is `'-' <?break> …`. Fix: lower
   any `<?name>`/`<!name>` naming a subrule to a `Lookaround` wrapping
   `Named(subrule)`. PR #5459, pin `t/regex-subrule-lookahead-assertion.t`.

## The indentation machinery (was the blocker; now resolved)

Every block collection goes through `root-block` / `block`:

```
token root-block {
    :my $new-indent;
    <?before $<sp>=[' ' ** { 0..* } ] { $new-indent = ~$<sp> }>
    $new-indent
    [ <value=sequence($new-indent)> | <value=map($new-indent)> ]
}
```

Dynamic quantifiers (`' ' ** { 0..* }`), `$<sp>=[...]` captures and separated
quantifiers (`+ % [ <.newline> $indent ]`) already worked; what did not is
below.

### (a) Match-time interpolation of a `:my`-declared regex var — DONE

```raku
grammar G { token TOP { :my $v = 'ab'; $v 'c' } }
G.parse("abc")   # now MATCHes
```

Implemented: a bare `$name` naming an in-regex `:my`/`:let` var lowers to
`RegexAtom::VarInterp(name)`, which at match time reads `caps.regex_vars` (then
`env`) and matches the string value literally (undefined → empty/zero-width),
mirroring `NamedBackref` (`src/runtime/regex/regex_match_capture.rs`). The parser
tracks `declared_regex_vars` (`regex_parse_core.rs`) and both interpolation
pre-passes (`interpolate_regex_scalars` in `regex_parse_modifier.rs`) leave a
declared name verbatim for this lowering. Pin: `t/regex-my-var-interpolation.t`.
Known gap: an outer lexical sharing the `:my` var's name can still be
pre-substituted on a secondary parse path (raku uses the `:my` shadow) — obscure,
not needed for YAMLish.

This does **not** unblock YAMLish by itself: `root-block` uses `:my $new-indent;`
with **no initializer** and computes the value in a code block (needs (b)).

### (b) Inline execution of `{ code }` side-effect blocks during matching — DONE

```raku
grammar L { token TOP { :my $x = 'n'; { $x = 'y' } <?{ $x eq 'y' }> 'z' } }
L.parse("z")     # now MATCHes
```

mutsu used to **defer** every plain `{ code }` block (stored in
`caps.code_blocks`, run after the match for `make`/side effects), so a
`{ $x = ~$<sp> }` write was invisible to the atoms after it. Implemented: a plain
block that needs nothing from the reduce walk is a pure side-effect block and now
runs inline, left-to-right, on the real interpreter — the same route ADR-0009
part B established for `<?{ … }>` — and is *not* recorded for the reduce-time
replay, so it still runs exactly once. Two constructs keep a block deferred,
because both need post-match ordering: **`make`** (a node's AST is built from its
already-reduced children) and a **dynamic variable** `$*x` (a rule's `:my $*x` is
one binding per match, installed/read back around each node's reduce step).
`code_block_defers_to_reduce` in `regex_helpers.rs` splits the two. Writes to the
in-regex `:my`/`:let` lexicals are harvested out of `env` and threaded back
through `RegexCaptures::regex_vars`, which `regex_trail` already undoes on
backtracking; writes to an *outer* lexical still reach the caller's compiled
slots via the reduce path's env-diff bookkeeping.

Three gaps fell out of it and were fixed in the same slice:

- `<?{ … }>` / `{ … }` never had `caps.regex_vars` installed in their env, so an
  assertion could not read a `:my` lexical at all.
- A lookaround / group / alternative is matched with a **fresh** capture store, so
  it neither saw the enclosing regex's `:my` lexicals nor propagated writes back
  out — which is exactly the shape `root-block` uses. A take-scoped
  `INLINE_REGEX_VARS_SEED` now seeds inline sub-patterns (and deliberately *not*
  subrules, which are a different regex), and those arms merge `regex_vars` out.
- `instantiate_named_regex_arg_calls` pre-rendered subrule **arguments** before
  the match, baking the not-yet-computed `Nil` into the pattern text permanently
  (`<value=sequence($new-indent)>`). An argument naming a `:my`/`:let` lexical is
  now left verbatim and re-evaluated at match time, and `make_regex_eval_env`
  installs the lexicals so it resolves.

Pin: `t/regex-inline-code-block.t` (11 tests, all also pass under raku).

### (c) Standalone `:` backtrack control — DONE

Unrelated to the code blocks, but the other half of what blocked YAMLish's map
path. `token key { <.plainfirst> : <-[\:\#]>* }` — a solitary `:` commits the
preceding atom — was rejected as "Unrecognized regex metacharacter :", killing the
whole rule. It now sets the per-token `ratchet` flag on the token just emitted
(`regex_parse_core.rs`); `::` / `:::` are left alone, and a `:` with no preceding
atom still errors as raku does. Pin: `t/regex-standalone-backtrack-control.t`.

## Remaining after the regex work — all three CLOSED (2026-07-28)

`YAMLish::Grammar.parse` produced raku's AST once the regex work landed; three
non-regex gaps stood between that and `load-yaml`, and all three are fixed:

1. ✅ **`.new` on a nested type.** Not grammar-specific: `module M { class A::B }`
   registered the ClassDef under the bare `A::B` while `.^name` said `M::A::B`, so
   `M::A::B.new` could not find its own definition. PR #5511,
   `t/nested-type-name-in-package.t`,
   `news/2026-07/nested-type-name-qualified-by-package.md`.
2. ✅ **An itemized list bound to a positional attribute.** `submethod
   BUILD(:@!elems)` stored `$(1,2)` straight through, so `@!elems` held the list
   itself and iterating it yielded that list.  An `@`-sigiled parameter now
   de-itemizes. `t/positional-param-deitemizes.t`.
3. ✅ **A user method losing to a same-named builtin.** The by-name dispatchers
   (`dispatch_method_by_name_1/2/3`) key on the method name alone and ran before
   user-method resolution, so YAMLish's action for its rule named `map` was
   answered by the collection builtin. `t/user-method-shadows-builtin-name.t`.

`load-yaml` now matches raku on every block collection:

```
- 1\n- 2    =>  [1, 2]
a: 1        =>  {a => 1}
a: 1\nb: 2  =>  {a => 1, b => 2}
- x\n- y    =>  ["x", True]
```

**What is left for the battery is packaging, not interpreter work**: vendor into
`modules/YAMLish/`, add the `batteries.lock` + `batteries-whitelist.txt` gate,
the wasm Batteries page row, flip `docs/batteries/yaml.md` to bundled, and index
it in BATTERIES.md §7. Worth a broader `load-yaml`/`save-yaml` sweep first (flow
collections, block scalars, anchors/aliases, multi-document) to find what else
the module exercises.

## Repro setup

`mutsu -I tmp/ybis -I modules/MIME-Base64/lib tmp/yseq.raku` (four block inputs).
Module tarball `~/.cache/mutsu-yaml-survey/yamlish/YAMLish-0.1.3`. Minimal repros:
`tmp/rb4.raku` (root-block shape), `tmp/cb_inline.raku` (inline blocks),
`tmp/y12.raku` (standalone `:`), `tmp/y13.raku` (TOP over all inputs),
`tmp/y6.raku` (the two concretize gaps). Emitter is `save-yaml` / `save-yamls`.

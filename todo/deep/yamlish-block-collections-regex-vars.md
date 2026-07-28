# YAMLish battery: block collections — regex side DONE, three downstream gaps left

**Status 2026-07-28.** The regex half of this ticket is finished: `(a)` and `(b)`
below are both implemented, plus two gaps they exposed. `YAMLish::Grammar.parse`
now returns the **byte-identical AST raku produces** for a block sequence
(`- 1\n- 2`, `- x\n- y`). What is left is no longer regex work — see
"Remaining after the regex work" at the bottom.



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

## Remaining after the regex work

`YAMLish::Grammar.parse("- 1\n- 2")` now yields the **same AST raku produces**,
so the grammar is no longer the blocker. `load-yaml` still fails, on three
independent non-regex gaps:

1. **`.new` on a grammar type object.** `Single.make-value` does
   `$schema.new.parse($!value)` where `$schema` is `Schema::Core` (a grammar).
   mutsu: `X::Method::NotFound: Unknown method value dispatch (fallback
   disabled): new on YAMLish::Schema::Core`. Repro: `tmp/y6.raku`.
2. **A positional attribute assigned an itemized list stays itemized.**
   `Actions::root-block` does `my ($class, $elems) = @($<value>.ast); $class.new(:$elems)`
   into `submethod BUILD(:@!elems)`. mutsu ends up with `@!elems` holding the
   itemized value: `.elems` reports 2 but `for $seq.elems` iterates **once**,
   yielding the `Array[Node]` itself, so `.map(*.concretize(…))` calls
   `concretize` on the Array. Raku de-itemizes into the positional. Repro:
   `tmp/y6.raku`; `tmp/y7.raku` shows the plain shapes all working, so it is
   specific to the `:$elems`-from-a-scalar path.
3. **The block `map` path throws.** `Grammar.parse("a: 1")` →
   `X::Cannot::Map: Cannot map a Any to a Package` (sequences are fine). Not yet
   root-caused. Repro: `tmp/y13.raku`.

## Repro setup

`mutsu -I tmp/ybis -I modules/MIME-Base64/lib tmp/yseq.raku` (four block inputs).
Module tarball `~/.cache/mutsu-yaml-survey/yamlish/YAMLish-0.1.3`. Minimal repros:
`tmp/rb4.raku` (root-block shape), `tmp/cb_inline.raku` (inline blocks),
`tmp/y12.raku` (standalone `:`), `tmp/y13.raku` (TOP over all inputs),
`tmp/y6.raku` (the two concretize gaps). Emitter is `save-yaml` / `save-yamls`.

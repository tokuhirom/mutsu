# YAMLish battery: block collections need match-time regex `:my` vars + inline code blocks

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

## Remaining blocker: the indentation machinery (two deep regex features)

Every block collection goes through `root-block` / `block`:

```
token root-block {
    :my $new-indent;
    <?before $<sp>=[' ' ** { 0..* } ] { $new-indent = ~$<sp> }>
    $new-indent
    [ <value=sequence($new-indent)> | <value=map($new-indent)> ]
}
```

Proof this is the *only* remaining gate: replacing the `:my $new-indent` +
interpolation with a param-passed empty indent (`<sequence('')>`) makes
`- 1\n- 2` parse in mutsu today (`tmp/noindent.raku` → MATCH). Dynamic
quantifiers (`' ' ** { 0..* }`), `$<sp>=[...]` captures, and separated
quantifiers (`+ % [ <.newline> $indent ]`) all already work. What does *not*
work:

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

### (b) Inline execution of `{ code }` side-effect blocks during matching

```raku
grammar L { token TOP { :my $x = 'n'; { $x = 'y' } <?{ $x eq 'y' }> 'z' } }
L.parse("z")     # raku: MATCH ; mutsu: NO ("Use of Nil in string context")
```

mutsu **defers** plain `{ code }` blocks (stored in `caps.code_blocks`, run after
match for `make`/side effects). Raku runs them **immediately, left-to-right,
during matching**, so a `{ $x = ~$<sp> }` write is visible to the following
`$new-indent` interpolation and `<?{…}>` assertions. This is the harder half:
side-effect blocks must execute inline and write to `caps.regex_vars`, while the
writes must be **undone on backtracking** (the `regex_trail` already snapshots
`regex_vars` — see `src/runtime/regex/regex_trail.rs:325`). `{ make … }` must
still behave as today (its result is carried at reduce time and must not change
the match). Distinguishing "side-effect block" from "make block" and threading
the writes through backtracking is the design work — likely ADR-worthy, high
blast radius (touches every regex with a `{ }` block).

## Order / recommendation

1. ✅ (a) DONE — `t/regex-my-var-interpolation.t`.
2. Then design (b): inline side-effect code blocks with backtrack-safe
   `regex_vars` writes, keeping `make` deferral intact. With (a)+(b),
   `Grammar.parse` of `- 1\n- 2` / `a: 1` should reduce, and `load-yaml` should
   round-trip block collections.

## Repro setup

`mutsu -I tmp/ybis -I modules/MIME-Base64/lib tmp/yseq.raku` (all four block
inputs → Failure today). Module tarball
`~/.cache/mutsu-yaml-survey/yamlish/YAMLish-0.1.3`. Minimal repros:
`tmp/rb3.raku` (a), `tmp/cb_inline.raku` (b), `tmp/noindent.raku` (proof the
rest works). Emitter is `save-yaml` / `save-yamls`.

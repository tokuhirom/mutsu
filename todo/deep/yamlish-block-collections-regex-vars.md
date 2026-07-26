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

### (a) Match-time interpolation of a `:my`-declared regex var as a literal

```raku
grammar G { token TOP { :my $v = 'ab'; $v 'c' } }
G.parse("abc")   # raku: MATCH ; mutsu: NO
```

`interpolate_bound_regex_scalars` (`src/runtime/regex/regex_interpolate.rs:247`)
substitutes `$var` from `self.env` at **pre-match** time. A regex-local `:my`
var isn't in `env` (it's set at match time by the `VarDecl` atom into
`caps.regex_vars`), so `$v` is left literal and then mis-parsed as the literal
text `$v` (or errors "Null regex"/"Use of Nil"). Bare `$var` interpolation must
become a **deferred** atom that at match time reads `current_caps.regex_vars`
(then `env`) and matches the string value literally, like `NamedBackref`
(`src/runtime/regex/regex_match_capture.rs:368`) does for `$<name>`.

Sketch: add `RegexAtom::VarInterp(String)`; in the parser track a
`declared_regex_vars: HashSet<String>` (populate from each `:my`/`:let`
`VarDecl`'s code — the name is the first `$ident` after `my `), and in Match mode
emit `VarInterp(name)` for a bare `$name` when `name` is in that set instead of
falling through to literal handling (`regex_parse_core.rs:875`). This alone is a
clean, general Raku feature worth landing even before (b) — but it does **not**
unblock YAMLish by itself, because `root-block` uses `:my $new-indent;` with **no
initializer** and computes the value in a code block (needs (b)).

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

1. Land (a) standalone first — general, self-contained, low risk. Pin:
   `:my $v = 'ab'; $v` matches, and `:my $v = ''; $v` matches empty.
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

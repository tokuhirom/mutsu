# A grammar rule with dynamic-variable parameters + `.parse(..., :args(...))` fails entirely

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/grammars.rakudoc:509`). The harness bucketed this as `raku-drift-from-doc`
because the doc's `# OUTPUT:` annotation lacks a leading space that real raku's actual
gist output has (a documentation typo, unrelated to this bug) — re-verified directly
against `raku` and this is a real, confirmed divergence, not drift.

## Root cause

Raku grammars can declare a rule/token with a parameter list that binds **dynamic**
variables (`rule TOP ($*word, $*extra) { ... }`), which `.parse()` fills in via the
`:args(...)` named argument. mutsu appears not to support this feature at all: the whole
parse fails silently, returning `Nil` instead of a `Match`.

## Minimal repro

```raku
grammar demonstrate-arguments-dynamic {
   rule TOP ($*word, $*extra) {
      <phrase-stem><added-words>
   }
   rule phrase-stem {
      "I like"
   }
   rule added-words {
      $*word $*extra
   }
}

say demonstrate-arguments-dynamic.parse("I like everything else",
  :args(("everything", "else")));
```

- `raku`:
  ```
  ｢I like everything else｣
   phrase-stem => ｢I like ｣
   added-words => ｢everything else｣
  ```
- `mutsu`: `Nil`

## Affected files (starting point)

- Parser: wherever a `token`/`rule`/`regex` declaration's parameter list is parsed —
  needs to accept dynamic-variable (`$*name`) parameters, not just ordinary ones.
- `src/runtime/methods_grammar.rs` / grammar `.parse()` dispatch — needs to accept the
  `:args(...)` named argument and bind it into the rule's declared dynamic parameters
  before running `TOP`.
- Compare against `grammar-token-param-dynvar-not-visible-in-subrule.md` (an existing
  ticket) — that one is about a *default-valued* `$*` token parameter not propagating to a
  called subrule; this finding is about a `.parse(..., :args(...))`-supplied `$*` parameter
  on the grammar's own `TOP` rule not working at all. They may share underlying
  infrastructure (how token/rule parameter lists bind into dynamic scope) but are distinct
  failure modes — investigate together if convenient.

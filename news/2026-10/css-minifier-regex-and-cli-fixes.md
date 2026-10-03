# Four fixes that make CSS::Minifier pass

The `CSS::Minifier` distribution went from 1 of 8 to 8 of 8 test files passing
under mutsu. It needed four independent fixes:

- **`<$var>` in a module's regex reads the module's own lexical.** The regex
  engine read an interpolated variable from the env alone. A module's
  file-scope `our $RE` is not there when the module's routine is called from a
  scope that never imported the module directly, so the match failed with
  `Variable '$NAMED-RE' is not declared`, or silently matched nothing. It now
  reads through `get_env_with_main_alias`, the by-name chokepoint a plain
  `$RE` read already uses.
- **`<?!x>` is `<!x>`.** The negative zero-width assertion spelled with both
  prefixes (`<?!alpha>`, `<?!before …>`, `<?!after …>`) never matched. It now
  parses as the negated form.
- **`.append` on a typed attribute array flattens before the type check.**
  `$rule.selectors.append: $other.selectors` with `has Str @.selectors` died
  with `expected Str but got Array`: the element check ran on the argument
  list instead of on the elements the one-arg rule actually appends.
- **An aliased named parameter's type applies to every alias.**
  `Int :l(:$level)` accepted `level => "foo"`, so `--level=foo` reached `MAIN`
  instead of printing usage.

`&*ARGS-TO-CAPTURE` is now the real default parser rather than an empty stub,
so a user `ARGS-TO-CAPTURE` can rewrite `@args` and delegate to it, as the
distribution's `cssminify` CLI does.

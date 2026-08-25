# The `s///` replacement is a `qq` quote, not a template language

Raku specifies the replacement half of `s///` / `S///` as a **`qq` quote**. mutsu
instead carried a hand-written interpolator in the VM
(`interpolate_subst_replacement_with_closures` plus a small family of scanners in
`vm_string_regex_ops.rs`) that implemented a *subset* of that grammar. Every
divergence any of the four `subst-*` doc-diff tickets recorded traced back to
that one decision:

- `$<name>` was not recognized at all, so
  `s/ $<y>=(\d+)\-$<m>=(\d+)\-$<d>=(\d+) /$<m>-$<d>-$<y>/` emitted its
  replacement literally (`$<m>-$<d>-$<y>` instead of `01-23-2016`).
- `%h{...}` / `%h<...>` was not recognized, so `S:g/@a/%h{$/}/` produced
  `%ha%hb` where raku gives `12`. (The *match* side of that ticket — an array
  interpolated as an alternation — already worked; only the replacement was
  wrong.)
- `@a[1]` interpolated the whole array rather than the element, and a bare `@a`
  interpolated at all, where raku leaves it literal (Raku only interpolates an
  array sigil that carries a postcircumfix).
- Backslash escapes were a short fixed list, so `\:` kept its backslash and
  `\c[LATIN SMALL LETTER Z]` was emitted verbatim.
- The `$/` a substitution published carried no named captures, so `$<n>` after
  `s/$<n>=(a)/x/` was `Nil`.

## The fix

The replacement source now goes through the *one* interpolation parser
(`parser::interpolate_qq_content`, the same entry `qq//` and heredocs use), and
the resulting expression is evaluated. `src/vm/vm_subst_repl.rs` holds the new
`SubstReplPlan`:

- a replacement that interpolates nothing is `Static` and is spliced in directly;
- otherwise it is `Dynamic`, evaluated once per match with `$/` (and the
  numbered capture env entries a `{ ... }` block reads) bound to that match.

The plan — parse tree, compiled-body cache id and all — is cached per
replacement source, so a `:g` substitution parses and compiles its replacement
once rather than once per match. Named captures reached the VM by adding
`regex_find_first_from_with_all_captures` alongside the positional-only variant,
and `:P5` patterns gained capture texts too
(`regex_find_all_p5_with_captures`), because raku treats a `:P5` replacement as
an ordinary `qq` quote and numbers its groups the Raku way (`$0` is PCRE group
1).

A `Dynamic` plan whose interpolations are nothing but capture references
(`$0`, `$<name>`, `$/`) also records them as a `Vec<ReplPart>` read straight off
that same parse tree, and splices those matches without entering the evaluation
carrier. It is a shortcut through the one grammar, not a second one: any part it
cannot supply — a capture the match did not produce, a multi-valued quantified
`$<name>` — makes it decline and that match falls back to evaluating the body,
so semantics (down to the `Use of Nil in string context` warning) stay those of
the single evaluator.

Net effect on the VM: `vm_string_regex_ops.rs` lost 260 lines of bespoke
scanning, `vm_subst_exec.rs` went from two near-duplicate 190-line functions to
one shared core, and both `s///` shapes measured faster than before
(a 4000-match `s:g/(x)/$0y/` 0.93s → 0.85s, a 4000-match `s:g/(x)/{$0}y/`
2.24s → 1.28s, debug build).

Pinned by `t/subst-replacement-interpolation.t`.

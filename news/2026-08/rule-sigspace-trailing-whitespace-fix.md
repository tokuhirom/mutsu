# `rule` (`:sigspace`) now consumes whitespace trailing the last atom

In Raku, `rule`/`:sigspace` inserts an implicit `<.ws>` between adjacent
atoms — including between the *last* literal atom and whatever whitespace
follows it in the pattern source, right up to the closing `}`. mutsu's
`rule`/`token`/`regex` body pipeline was losing that trailing whitespace
before the implicit-`<.ws>` injection pass (`inject_implicit_rule_ws`,
`src/parser/stmt/class/token_body.rs`) ever saw it, in three separate
unconditional-trim sites:

- `parse_raw_braced_regex_body` — extracts the `{ ... }` body source text and
  called `body.trim()`, dropping trailing whitespace immediately.
- `normalize_token_pattern` — the follow-up normalization pass, whose
  non-`/…/`-wrapped branch returned `trimmed.to_string()` (the fully-trimmed
  copy) instead of the original string.
- `inject_implicit_rule_ws`'s own per-whitespace-run loop required BOTH a
  preceding *and* a following non-whitespace atom
  (`if let (Some(p), Some(n)) = (prev, next)`) before emitting `<.ws>`, so a
  trailing run with nothing after it (right before the closing `}`) fell
  through and produced nothing at all.

All three needed a fix together — preserving the trailing whitespace through
extraction and normalization does nothing if the injection pass itself can't
act on a run with no following atom. Fixed:

```raku
grammar G { rule r { 'a' 'b' } }
my $m = G.subparse("a b   c", rule => 'r');
say $m.to;                    # was 3, now 6 (matches raku)
say "a b   c".substr($m.to);  # was "   c", now "c"
```

A plain `token`/`regex` (no `rule`, so `inject_implicit_rule_ws` never runs)
is unaffected either way: inter-atom pattern whitespace — including a
trailing run before `}` — is always pure layout, never a matchable atom,
regardless of ratchet/sigspace mode (whitespace between two adjacent literal
atoms, quoted or bare, is source-level syntax, not pattern content).

This was blocking `Template::Mojo`'s `perlcapture-begin`/`perlcapture-end`
rules, which rely on trailing sigspace to skip the literal newline right
after `<% ... begin %>` / `<% end %>` — without it mutsu emitted spurious
`$_M ~= '\n'` literals into the generated template sub.

Verified against the ticket's two repros (both now match `raku` exactly),
the new `t/rule-sigspace-trailing-whitespace.t`, all 183 local
grammar/token/rule/regex/sigspace test files, and the full local `t/` suite
(29,577 tests, no regressions).

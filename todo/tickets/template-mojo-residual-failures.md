# Template::Mojo: residual failure after the arity-message and Nil-capture fixes

Split out of `todo/tickets/grammar-named-capture-resolved-as-method.md` on
2026-07-26. That ticket's root cause — a quoted `<`/`>` inside a regex assertion
breaking the parse — is fixed (`news/2026-07/regex-assertion-quoted-angle-brackets.md`),
taking `Template::Mojo` 0.2.2 from every test file dying immediately to:

| file | mutsu | raku |
| --- | --- | --- |
| `t/00-basic.rakutest` | 16/17 (was 15/17) | 17/17 |
| `t/01-template.rakutest` | 3/3 | 3/3 |
| `t/02-complex.rakutest` | 1/1 | 1/1 |
| `t/03-capture.rakutest` | 0/1 | 1/1 |
| `t/04-native-named.rakutest` | 1/1 | 1/1 |

Reproduce by unpacking the dist and running `mutsu -I lib t/<file>` (the tarball
URL is in the git history of the original ticket).

## 1. `00-basic` test 16 — FIXED 2026-08-06; test 17 stays open (by decision)

Was: a named sub built via EVAL (`sub t { $^a + $^b }`), called through a
lexical value (`&f(23)`), reported the compile-time-flavored "Calling
t(Int) will never work with declared signature ()" instead of raku's plain
runtime "Too few positionals passed; expected 2 arguments but got 1". Two
independent bugs, both in the legacy placeholder binder that this call shape
falls into (`bind_function_args_values` in
`src/runtime/types/binding_signature.rs`, reached via the "compile on-the-fly"
value-call branch in `src/vm/vm_dispatch_helpers.rs`):

1. That value-call branch never set `suppress_binding_error_enhance`, unlike
   its sibling branches, so `enhance_binding_error` wrapped the raw runtime
   error in the "will never work" compile-time phrasing (and with an empty
   `()` signature, since placeholder params never populate `param_defs`).
2. The legacy binder's too-few message didn't match raku's wording
   ("Missing required implicit placeholder parameter $^b" instead of "Too few
   positionals passed; expected N arguments but got M").

Both fixed; regression test `t/eval-named-sub-placeholder-arity.t`.

**Test 17 ("too many arguments") stays failing, by decision, not oversight.**
raku also rejects extra positionals for this exact shape, but the general fix
was tried and reverted: mutsu's legacy binder has no reliable signal to tell
"a `^`-twigil placeholder sub with an exact arity" apart from "one whose body
also references bare `@_`/`%_`, which legitimately accepts extra positionals
in Raku" (`t/placeholder.t`'s `mixed-placeholder` sub is exactly the second
shape, and regressed under the first attempt). The only place that
distinction is knowable is at parse time (does the body reference `@_`/`%_`),
and the only way found to carry it to the runtime binder was a synthetic
`params` entry — which leaks into the ~80 other call sites across the
codebase that read a Sub's raw `params` list verbatim (multi-dispatch
candidate arity matching in `methods_signature_candidates.rs` was the one
caught by inspection; auditing all ~80 was out of scope for this ticket). See
the comment above `required_positional_count` in `binding_signature.rs` for
the full reasoning. A real fix needs a dedicated field threaded from the AST
through to the runtime `Sub` value, not a `params` list hack.

## 2. `03-capture` — two layers, one fixed, one still open

```
Use of Nil in string context
  in sub expr at lib/Template/Mojo.rakumod line 72
```

`method perlline($/) { make expr($/) ~ "\n" }` — the helper `expr` reads
`$<get-result>` / `$<expr>` off the match, from the `perlline` token
(`^^ \h* '%' $<get-result>=['=']? $<expr>=[ <-[\n]>* ] [\n | $]`).

**Layer 1 (FIXED 2026-08-06):** `$<get-result>=['=']?` unmatched was
rendering as `Nil` instead of Raku's empty (zero-width) `Match` — the
Nil-vs-empty-Match choice depends on whether the `?` quantifies the SAME
token the name is attached to (empty Match) or a `CaptureGroup` atom (still
Nil, matches Raku). Fixed in `src/runtime/regex/regex_match_core.rs`
(`RegexQuant::ZeroOrOne`'s three zero-match branches); see
`news/2026-08/regex-token-named-optional-atom-empty-match-not-nil.md` and
regression test `t/regex-optional-named-capture-nil-vs-match.t`. This also
surfaced a second, unrelated pre-existing bug
(`todo/tickets/named-capture-absent-from-current-match-leaks-stale-value.md`)
that the new test deliberately routes around (distinct capture names per
scenario).

**Layer 2 (still open):** with the Nil warnings gone, the file still fails —
not from a wrong value, but from extra blank lines in the rendered output.
Root-caused to `todo/tickets/rule-sigspace-does-not-consume-trailing-whitespace.md`:
`Template::Mojo`'s `perlcapture-begin`/`perlcapture-end` rules rely on
`rule`'s `:sigspace` consuming the newline right after `<% ... begin %>` /
`<% end %>`, and mutsu's regex engine never emits that trailing `<.ws>` after
a rule's last atom. That is a general grammar-engine gap, not specific to
this dist, and is the actual blocker for `03-capture.rakutest`.

## Why they were separate

All three findings are independent: the first is placeholder/arity plumbing
through EVAL (fixed), the second is capture population for an optional
capture whose `?` sits on the named token itself (fixed), and the third is a
`rule`/`:sigspace` boundary gap (open, tracked in its own ticket since it's a
general engine issue, not Template::Mojo-specific).

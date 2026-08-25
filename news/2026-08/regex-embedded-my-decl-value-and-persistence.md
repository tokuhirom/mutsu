# Fix embedded regex `:my $var = EXPR;` reading an unbound `$0` and not persisting to the caller

A regex-embedded `:my $var = EXPR;` declarator that references the in-progress
match state read an unbound value and never survived past the match:

```raku
"aba" ~~ / (a) {say "Check so far ", ~$/} b :my $c = ~$0; /;
say "Capture $c";
```

- raku: `Check so far a` then `Capture a`
- mutsu (before this fix): `Check so far a` then `Capture` (empty, with a "Use
  of Nil in string context" warning)

This is a distinct bug from the one fixed in
`news/2026-08/regex-decl-self-referential-slash.md` (that fix only addressed a
*parse* failure — "Regex not terminated" — for a `:my $c = $/;` declarator; it
explicitly noted this value/persistence bug as separate and untouched). It is
also distinct from the leading-declarative-prefix `:my`/`:constant`
persist-to-caller-scope fix landing separately in PR #6964 — that PR only
covers a `:my` at the very *front* of a pattern (before any other atom);
mid-pattern (embedded) `:my` had no persistence path at all before this fix,
regardless of whether its value was captured correctly.

## Root cause 1: the declarator's RHS was evaluated with capture state unbound

The plain `{ ... }` code-block atom (`RegexAtom::CodeAssertion` in
`src/runtime/regex/regex_match_capture.rs`) already installs `$0`, `$1`, ...,
named captures, and `$/` into the interpreter environment before running its
body (`eval_regex_inline_code` in `src/runtime/regex/regex_eval.rs`) — that is
why `{say "Check so far ", ~$/}` correctly sees the match-so-far text.

The embedded `:my $var = EXPR;` declarator (`RegexAtom::VarDecl`, handled a
few hundred lines below in the same file) evaluated its initializer `EXPR`
directly against `self`/a scratch interpreter, but only installed the
regex's own previously-declared `:my`/`:let` lexicals (`regex_vars`) into the
env first — never the positional/named captures or `$/`. So `~$0` read an
unbound value and the declarator's value ended up empty.

The fix factors the capture-binding logic `eval_regex_inline_code` already
uses into a new helper, `Interpreter::regex_capture_bindings` (in
`regex_eval.rs`), and calls it from the `VarDecl` arm before evaluating the
initializer (in both its real-interpreter fast path for simple non-dynamic
declarations and its scratch-interpreter path for dynamic ones), restoring
the installed bindings before the arm returns. The helper deliberately skips
the `.made` grammar-action dispatch `eval_regex_inline_code` also performs —
running a grammar action as a side effect of what looks like a plain variable
read would be surprising.

## Root cause 2: no caller-scope persistence for a mid-pattern `:my`

Per `raku-doc/doc/Language/regexes.rakudoc`: "`:my` helps scoping the `$c`
variable within the regex and beyond" — its value must persist into the
caller's enclosing lexical scope after a successful match, wherever in the
pattern it appears. The existing persistence mechanism
(`regex_match_with_captures` in `src/runtime/regex/regex_match_public.rs`)
only handled a *leading* declarative-prefix `:my`/`:let`/`:constant`/`:temp` —
declarators text-scanned off the very front of the pattern before the real
match runs. A mid-pattern `:my`, parsed instead as a `RegexAtom::VarDecl` atom
reached during the normal match walk, only accumulated into the winning
match's `RegexCaptures.regex_vars` (used for `<{ ... }>` interpolation and
`make`-block replay within the *same* match) — nothing ever wrote it back to
the caller afterward.

Added `Interpreter::persist_embedded_my_decls`, called after a successful
match (both in `regex_match_with_captures`'s fast path with no leading
declarators, and after its existing leading-declarator handling, skipping
names the leading-declarator path already covers) — it installs every
remaining `regex_vars` entry into `self.env` and logs it via the same
`carrier_writes`/`pending_local_updates` bookkeeping the existing `:let`
persistence already uses, so the smartmatch/match-operator call site's
`writeback_match_locals` reconciles the caller's compiled local slot.

## Testing

Added `t/regex-embedded-my-decl-value.t` (8 assertions, cross-checked against
real `raku`): the exact raku-doc worked example, a `$0`-derived value visible
to a later code block within the same match, `:my $var = ~$/;` reading the
whole match-so-far text directly, two chained `:my` declarators binding `$0`
and `$1` independently, and a smoke test that a non-matching pattern with an
embedded `:my` does not crash.

## Known follow-up (not fixed here)

Real raku only makes `$0`/`$/` visible to a `:my` declarator's initializer if
an earlier code block (even an empty `{}`) has already "published" them in
the same pattern — `raku-doc` documents this explicitly: "the match variable
`$/` or numbered matches such as `$0` will not be available [to `:my`] unless
they are previously published by inserting the empty block (or any block)".
This fix installs the capture bindings unconditionally (matching the plain
`{ ... }` code block's own unconditional behavior), so mutsu is slightly more
permissive than raku when a `:my $c = ~$0;` has no preceding block at all —
a narrow discrepancy, not a regression (previously `$0` was *never* visible to
`:my`, in every case). Replicating the "publish" gate exactly would need
tracking, per `VarDecl` atom, whether an earlier code-block atom already ran
in this match attempt — a small follow-up if it turns out to matter for real
code.

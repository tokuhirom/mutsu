# A lone `'` in a `< ... >` word list is a word, not a quote delimiter

A `< a b c >` alternation inside a regex lists literal words, and a lone `'` (or
`"`) there is an ordinary one-character word. Three scanners toggled quote state
on it and then treated the whole rest of the pattern as quoted:

```raku
say ("!#" ~~ / ^ < ! ' # >+ /).pos;   # was 1, raku says 2
grammar B { token TOP { < ! ' # > || <[A..Za..z]> } }
say B.parse("t").defined;             # was False, raku says True
```

Two distinct symptoms, one cause:

1. **The quantifier vanished.** The `<...>` assertion-body scanner in
   `regex_parse_core.rs` opened a quote on the `'`, never found a closing one, and
   so ran past the assertion's own `>` to the end of the pattern. The body came
   out as `` ! ' # >+ `` — the closing `>` and the `+` were absorbed into the word
   list — so the alternation carried no quantifier and matched exactly once, for
   *any* input.
2. **`|` / `||` stopped splitting.** `split_top_level_alternation` and
   `split_top_level_conjunction` in `regex_parse_ltm.rs` toggled the same way, so
   every branch operator after the word list looked like it was inside a string
   and the pattern was never split.

Both were invisible unless the quote count in the pattern was odd — `< ! ' ' # >`
happened to re-balance and worked.

## Fix

- **Assertion body:** scan honouring quotes first; if the scan reaches the end of
  the input with a quote still open, that `'`/`"` was never a quote opener, so
  re-scan with quote tracking off. A properly closed quote keeps the previous
  behaviour exactly. The scan loop moved into a reusable
  `scan_angle_assertion_body` helper to make the two passes possible.
- **Alternation/conjunction splitters:** pause quote tracking while inside an
  angle assertion (`depth_angle > 0`). This is safe because the `|` and `&` split
  arms already require `depth_angle == 0`, so an assertion's contents can never
  produce a spurious split and need no quote tracking at all.

## Why it matters

This is the HTTP `tchar` production, which `HTTP::MediaType` uses verbatim:

```raku
token tchar { || < ! # $ % & ' * + - . ^ _ ` | ~ > || <.DIGIT> || <.ALPHA> }
```

Every media type failed to parse, so `HTTP::Response.is-text` threw
`X::MediaTypeParser::IllegalMediaType` on a plain `text/html` response — the
third blocker found while driving `HTTP::UserAgent` against a live server. It now
returns `True`, matching `raku`.

Pin: `t/regex-angle-word-list-bare-quote.t` — 14 assertions that pass identically
under `raku` and mutsu.

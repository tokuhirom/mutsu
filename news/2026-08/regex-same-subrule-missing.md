# `<same>` is a zero-width "adjacent characters are equal" assertion, not a backreference

The ticket that filed this (from the doc-diff harness on
`Language/regexes.rakudoc:1349`) guessed `<same>` was "a backreference-like
assertion that matches only if it repeats the immediately preceding capture's
matched text". Measured against `raku` v2026.06, it is nothing of the sort:

```raku
say '123345' ~~ m/ <same>\d+ /;   # ｢345｣  with  same => ｢｣
say 'aa11'   ~~ m/ <alpha><same><digit> /;   # False
say 'abab'   ~~ m/ (ab) <same> /; # False
```

`<same>` is **zero-width** and succeeds exactly where the character *before*
the current position equals the character *after* it. In `123345` that is the
position between the two `3`s, so the following `\d+` matches `345`. It needs a
character on both sides, so it fails at the start and at the end of the string.

mutsu already had the machinery: `RegexAtom::SameAssertion` implemented exactly
these semantics for the `<?same>` / `<!same>` spellings. Only the **bare** name
was unrouted, so `<same>` fell through to the generic subrule path and died with
`No such method 'same' for invocant of type 'Match'`. The same hole existed for
bare `<wb>` and `<ww>` (whose `<?wb>` / `<?ww>` / `<!ww>` forms were added the
day before in the `RegexAtom::WithinWord` change).

## The fix

`regex_parse_core.rs` now recognises `same` / `wb` / `ww` (and the
non-capturing `.same` / `.wb` / `.ww`) as the assertions they are, unless the
current grammar defines a token of that name — the same shadowing rule
`<alpha>` already obeyed, factored out as `regex_name_is_grammar_token`.

The bare spelling also had to **publish a named capture**, because that is what
distinguishes it from `<?same>`:

```raku
my $m = 'aa' ~~ m/ . <same> /;
say $m<same>.from;   # 1   -- a zero-width Match at the assertion position
```

That reuses the existing `pending_builtin_named_capture` channel, the one
`<alpha>` / `<digit>` use to make `$<alpha>` work; `<.same>` suppresses it.

Pinned by `t/regex-engine-gaps.t`.

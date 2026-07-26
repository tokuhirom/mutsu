# `Q:to/…/` is verbatim again — no `\qq[…]`, no escape processing

`Q` is the *no-escapes, no-interpolation* quoting form: inside `Q`, a `\qq[…]`
sequence is literal text. mutsu processed it as an interpolation escape, the way
it correctly does for `q` and `qq`:

```raku
my $name = 'world';
say Q:to/END/;
raw \qq[$name] here
END
# raku:  raw \qq[$name] here
# mutsu: raw world here
```

## Scope correction

The ticket expected `Q[…]`, `Q{…}` and `Q:to/…/` to all be affected. Measuring
showed only the **heredoc** form was wrong — the delimited forms already treated
the escape as literal. So the fix is confined to the heredoc path, and the
delimited forms are pinned so they cannot drift away from it.

## The fix

`parse_to_heredoc_with_flags` (`parser/primary/string/heredoc.rs`) ran the `q`
escape rules for any non-interpolating heredoc: `\qq[…]` re-entered
interpolation, and `\\` collapsed to `\`. Both are `q` semantics. The arm now
tests `flags.q_mode` — which `q:to` sets and `Q:to` does not — and a `Q` heredoc
takes its content verbatim. Adverbs on `Q` are unaffected: `Q:qq:to/…/` still
interpolates, because it comes through the `interpolate` / flags branches above.

Pin: `t/q-heredoc-no-escapes.t` — 10 assertions verified against raku first:
`Q:to` leaving `\qq[…]`, `\\`, `\n` and `$var` literal; `q:to` still honouring
`\qq[…]` and collapsing `\\`; `Q:qq:to` and `qq:to` still interpolating; and the
three delimited forms.

(A note for whoever writes the next pin: a *single-quoted* expected value is the
wrong way to spell one of these strings — `'raw \qq[$name] here'` interpolates,
because `q` semantics honour `\qq[…]` there too. Use a double-quoted string with
explicit escapes.)

## Why it matters

`Q:to/…/` with a `\qq[…]`-looking body is what a template engine emits when it
generates Raku source — this was found while reducing `Template6`, whose
`Parser.compile` builds its generated program out of nested `q:to`/`Q:to`
heredocs. Note that it is **not** the `Template6` blocker:
`todo/deep/template-engines-blocked-on-mutsu.md` records that the module's
failing heredocs are `q:to`, which mutsu already handled correctly. This is a
real bug on its own.

# `rule` (`:sigspace`) does not consume whitespace trailing the last atom

Found while chasing the remaining `Template::Mojo` `03-capture.rakutest`
failure (`todo/tickets/template-mojo-residual-failures.md` issue 2 turned out
to have two layers; the Nil-vs-empty-Match layer is fixed, this is the rest).

In Raku, `rule`/`:sigspace` inserts an implicit `<.ws>` between adjacent atoms
-- including between the *last* literal atom and whatever whitespace follows
it in the pattern source, right up to the closing `}`. mutsu's regex parser
inserts the between-atom `WsRule` tokens (`src/runtime/regex_parse_core.rs`,
the `c.is_whitespace()` branch around line 731) but the trailing whitespace
after the final atom is apparently never reached — matching consistently
stops immediately after the last literal, never consuming what follows.

Minimal repro (`subparse` used to inspect `.to` without needing an anchor):

```raku
grammar G {
    rule r { 'a' 'b' }
}
my $s = "a b   c";
my $m = G.subparse($s, rule => 'r');
say "to=" ~ $m.to;
say "rest=[" ~ $s.substr($m.to) ~ "]";
```

raku: `to=6` / `rest=[c]` (all three trailing spaces consumed by the
post-'b' `<.ws>`, which is `\s+` after a word character).
mutsu: `to=3` / `rest=[   c]` (stops right after `'b'`, no trailing `<.ws>`
at all).

The same gap reproduces with a name-and-quote-delimited case closer to the
original find:

```raku
grammar G {
    rule perlcapture-begin {
        '<%' 'my' $<name>=<var> '=' 'begin' '%>'
    }
    token var { <sigil> [ \w+ ] }
    token sigil { '&' | '$' }
}
my $s = "<% my &block = begin %>\nHello";
my $m = G.subparse($s, rule => 'perlcapture-begin');
say "to=" ~ $m.to;          # raku: 24 (consumes the trailing \n)  mutsu: 23
```

## Impact

`Template::Mojo`'s `perlcapture-begin`/`perlcapture-end` rules are declared
with `rule` specifically to get this behavior (skip the literal newline right
after `<% ... begin %>` / `<% end %>`), so without it mutsu emits spurious
`$_M ~= '\n'` literals into the generated template sub, producing extra blank
lines in rendered output. Reproduced end-to-end via
`~/.zef/store/Template-Mojo-0.2.2/*/t/03-capture.rakutest`
(`mzef install Template::Mojo` first, or unpack the dist tarball).

This is a general `rule`/`:sigspace` gap, not Template::Mojo-specific — any
grammar relying on trailing sigspace after a rule's last atom will hit it.

## Where to look

`src/runtime/regex_parse_core.rs`'s per-character tokenizer loop (the
`c.is_whitespace()` branch, currently only inserts a `WsRule` token when
`chars.peek()` finds more pattern text after the whitespace run — or possibly
the token/rule BODY TEXT itself is trimmed of trailing whitespace before
reaching this parser at all, in the caller that extracts `{ ... }` body
source for `token`/`rule`/`regex` declarations). Check both: (1) whether the
extracted body string for a `rule { ... }` still contains its trailing
whitespace before `}` when handed to the regex parser, and (2) whether the
tokenizer, when it does see trailing whitespace with no more atoms following,
still emits a `WsRule` (it should — a `RegexToken` doesn't need "something
after" to be meaningful, it just needs to be walked in `walk_tokens`).

## Effort

Not measured; the tokenizer-vs-body-trimming split needs an hour or two of
tracing before the actual fix site is clear, so likely S-M once diagnosed.

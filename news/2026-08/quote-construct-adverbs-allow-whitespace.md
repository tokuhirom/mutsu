# Whitespace is allowed between a quote-like operator and its adverbs

`$str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;` was a hard compile error:

```
Confused. expected statement: expected expression statement or expression after additive operator ...
------>$str ~~ s :g :i/<[ML]> (\S+)/d{lc $0}/;
```

The ticket that recorded this blamed the replacement text — a literal `d`
immediately followed by the code block `{lc $0}`. That diagnosis was wrong:
`s:g:i/<[ML]> (\S+)/d{lc $0}/`, written with the adverbs tight against the `s`,
already parsed and evaluated correctly. The actual trigger was the **space
between `s` and `:g`**.

Raku allows whitespace between a quote-like operator and its adverbs, and
between consecutive adverbs — `s :g :i/…/…/`, `m :i /B/`, `S :g/…/…/`,
`tr :d/b//` all work in rakudo. mutsu's `parse_match_adverbs` ended its loop the
moment the input did not start with `:`, and the `s`/`ss` entry points only
called it at all when the character right after the keyword was `:`.

The adverb loop now skips leading whitespace, but only when an adverb genuinely
follows (`skip_ws_before_adverb`): a plain `s /pat/repl/` must keep its
whitespace visible, because the callers use it to decide whether `(` opens a
delimiter or a call, and `m ::Foo` must stay a package name. The same helper is
wired into `parse_trans_adverbs`, so `tr`/`TR` accept spaced adverbs too.

Pinned by `t/subst-replacement-interpolation.t` (the spaced-adverb cases for
`s`, `S`, `m` and `tr`, plus the original ticket's full repro).

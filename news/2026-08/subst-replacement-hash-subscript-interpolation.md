# `S:g/@a/%h{$/}/` — the array alternation was fine, the replacement was not

```raku
my %h = a => 1, b => 2;
my @a = %h.keys;
say S:g/@(%h.keys)/%h{$/}/ given 'abc';   # raku: 12c, mutsu: %ha%hbc
say S:g/@a/%h{$/}/ given 'abc';           # raku: 12c, mutsu: %ha%hbc
```

The ticket suspected the *match* side — that `@array` / `@(EXPR)` interpolated
into a regex was not being expanded into an alternation. Reading mutsu's own
output disproves that: `%ha%hbc` contains one replacement per matched letter, in
order, so the alternation matched `a` and then `b` exactly as it should. The
whole error was on the replacement side, where `%h{$/}` was lexed as the literal
text `%h` followed by an interpolated `{$/}` block — because the replacement's
hand-written interpolator knew about `$var` and `{ ... }` but not about the
`%`/`@` sigils and their postcircumfixes.

The replacement is a `qq` quote and now parses as one, so `%h{$/}`, `%h<a>` and
`@a[1]` are subscript interpolations like they are in `"..."`. See
[subst-replacement-is-a-qq-quote.md](subst-replacement-is-a-qq-quote.md).

The one genuinely separate finding here — that `$/` written directly before the
closing delimiter (`s/(a)/[$/]/`, `s:g/x/$//`) still fails to lex — is recorded
in [subst-replacement-slash-var-before-delimiter.md](subst-replacement-slash-var-before-delimiter.md);
it lived in the delimiter scanner, not in the interpolation grammar, and has
since been fixed there.

Pinned by `t/subst-replacement-interpolation.t`.

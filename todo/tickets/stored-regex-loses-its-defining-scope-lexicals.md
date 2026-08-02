# A stored regex loses its defining scope's lexicals when it escapes the sub

Extracted from PLAN.md §1 B4 (2026-08-02) and re-verified on `main` the same day. Originally found
while running the (now-retired) Tubu web framework; it is a general interpreter bug on its own axis.

## Repro

```raku
sub make() {
    my $word = 'abc';
    return rx/ $word /;
}
my $r = make();
say ("xx abc yy" ~~ $r).defined;   # mutsu: False    raku: True
```

The regex is built inside `make`, interpolates the lexical `$word`, and is then returned. By the
time it runs, mutsu no longer resolves `$word`, so the match fails. `b07ee6627`
("feat(runtime): a regex literal captures its defining scope") fixed the same-scope case; this
escaping-the-frame case still fails, so the capture is not surviving the frame teardown.

## Second, related divergence in the same area

A stored regex used as a `<$var>` assertion leaks its inner captures into the *outer* match's
positional slots:

```raku
my $inner = rx/ (\d+) /;
my $outer = rx/ 'n=' <$inner> /;
"n=123" ~~ $outer;
say ($/[0] // 'undef');   # mutsu: ｢123｣     raku: undef
```

In Rakudo a `<$var>` assertion does not publish the sub-regex's positional captures into the calling
match (both agree that `$/{'$inner'}` is undefined). mutsu splices them in as `$/[0]`.

## Affected files

`src/runtime/regex.rs` / `regex_parse*.rs` (assertion handling for `<$var>`), and whatever performs
the defining-scope capture for a regex literal (see `b07ee6627` and
`news/2026-08/regex-literal-captures-defining-scope.md` if present).

## Why it is not a one-liner

The first half is the closure-capture-lifetime family (the captured env must outlive the frame that
built the regex, like a `Sub`'s closure), and the second half is capture-namespace plumbing through
assertion invocation — two different mechanisms that happen to meet in the same construct.

# A custom `postcircumfix:<[...]>` operator's `+@slurpy` args are wrong when called via subscript syntax (but correct as a plain sub call)

Discovered via the doc-diff harness on `raku-doc/doc/Language/faq.rakudoc` (around line 359).

## Repro

```raku
multi postcircumfix:<[- ]> (Str:D $str is copy, +@indices) {
    for @indices.reverse {
        when Int   { $str.substr-rw($_,1) = '' }
        when Range { $str.substr-rw($_  ) = '' }
    }
    return $str;
}

say '0123456789'[- 1..3, 8 ];
```

- `raku`: `045679`
- `mutsu` (`target/debug/mutsu`): `(Index out of range. Is: 1, should be in 0..0 Index out of
  range. Is: 8, should be in 0..0)` — a `Failure`/error object is printed instead of the mutated
  string, meaning the `substr-rw` calls inside the loop failed against out-of-range indices.

## Isolated: the identical logic works fine as a plain sub call

```raku
sub foo(Str:D $str is copy, +@indices) {
    for @indices.reverse {
        when Int   { $str.substr-rw($_,1) = '' }
        when Range { $str.substr-rw($_  ) = '' }
    }
    return $str;
}
say foo('0123456789', 1..3, 8);   # 045679 -- matches raku, both with mutsu and raku
```

This is confirmed correct in mutsu when called as a normal sub `foo('0123456789', 1..3, 8)`. It
is *only* wrong when the exact same signature/body is declared as `multi
postcircumfix:<[- ]>` and invoked via the subscript syntax `'...'[- 1..3, 8]`. Also confirmed
each individual step works when done manually in sequence at the top level (`$str.substr-rw(8,1)
= ''` then `$str.substr-rw(1..3) = ''` on the same string both give the correct `045679`), which
rules out a `substr-rw`/`when Int`/`when Range` dispatch bug in isolation.

So the bug is specific to how mutsu passes/threads the postcircumfix-subscript's bracket
arguments into the `+@indices` slurpy parameter when dispatched through custom
`postcircumfix:<...>` operator syntax, as opposed to an ordinary named-sub call with the same
arguments.

## Affected files (starting point)

- Wherever a custom `postcircumfix:<...>` operator's subscript-bracket contents are parsed and
  turned into an argument list for the underlying multi-sub call (likely in `src/parser/` for
  the postcircumfix-subscript grammar, and/or `src/compiler/` for how that argument list is
  compiled/passed) — compare against the normal call-argument-list compilation path that the
  plain-sub-call repro above goes through correctly.

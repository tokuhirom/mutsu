# A user-defined custom infix's RHS operand fails to parse when followed by `??...!!`

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/experimental.rakudoc:144`).

## Root cause hypothesis

When the right-hand operand of a user-defined custom infix operator
(`sub infix:<name>(...) {...}`) is an expression that continues into a ternary
(`??...!!...`), mutsu fails to parse it at all, even though the same shape parses fine
when the LHS is a bare literal instead of going through a custom infix. This looks like the
custom-infix operand parser uses a narrower/lower-precedence term-parsing routine than the
general expression parser (one that doesn't include `??`/`!!` as a valid continuation),
so it stops after consuming just the immediate term and then chokes on the leftover `??`.

## Minimal repro

```raku
sub infix:<amic>( $m, $n ) { $m == $n }
my @pair = (2, 2);
say 2 amic @pair[1]??" yes"!!"no";
```

- `raku`: parses fine, `amic`'s default (loose, comma-like) precedence means the ternary
  binds as `2 amic (@pair[1] ?? " yes" !! "no")`.
- `mutsu`: `Runtime error: Expected a term, but found either infix ?? or redundant prefix
  ?  (to suppress this message, please use a space like ? ?)`

For comparison, ternary alone (no custom infix in the way) parses fine on both:

```raku
my $x = 5;
say $x > 3??" yes"!!"no";   # both raku and mutsu: " yes"
```

This is how it surfaces in the doc's `:cached`/amicable-numbers example
(`experimental.rakudoc`): `@pair[0] amic @pair[1]??" "!!"not ", "amicable"` — the `:cached`
trait itself is unrelated; the crash is purely this custom-infix-operand parsing gap.

## Affected files (starting point)

- Parser: wherever a user-declared custom infix operator's right-hand operand is parsed
  after the operator token is recognized — grep for custom infix / `infix:<...>` operand
  parsing in `src/parser/expr/`. It likely needs to call the same term/expression parser
  used for the RHS of built-in infix operators (which already accepts a following ternary)
  instead of a narrower one.

# A hyper-operator wrapping another hyper-operator (`»>>+<<»`) fails to parse

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/operators.rakudoc:567`).

## Root cause hypothesis

Raku allows an outer hyper-op (`»...«`) whose wrapped operator is itself already a hyper form
(`>>+<<`, i.e. `«+»` written in the ASCII `<<...>>` style), used to broadcast a nested-tuple
element-wise binary operation across two nested-tuple lists:

```raku
my $neighbors = ((-1, 0), (0, -1), (0, 1), (1, 0));
my $p = (2, 3);
say $neighbors »>>+<<» ($p, *);   # raku: ((1 3) (2 2) (2 4) (3 3))
```

mutsu fails to parse this at all — `===SORRY!=== ... Confused. expected expression after hyper
operator or '.' or digits ...`. The hyper-operator grammar apparently only recognizes a single
plain infix operator token between the `»`/`«` delimiters (e.g. `»+»`, `»>>+<<»` is presumably
being tokenized in a way where the parser doesn't expect `>>+<<` — a full nested hyper spelling
— as the "operator" term inside the outer hyper brackets.

## Minimal repro

```raku
my $neighbors = ((-1, 0), (0, -1), (0, 1), (1, 0));
my $p = (2, 3);
say $neighbors »>>+<<» ($p, *);
```

- `raku`: `((1 3) (2 2) (2 4) (3 3))`
- `mutsu` (`target/debug/mutsu`): compile-time parse error:
  ```
  ===SORRY!=== Error while compiling ...
  Confused. expected expression after hyper operator or '.' or digits or generic radix literal or unicode numeric literal or ...
  ```

## Affected files (starting point)

- The hyper-operator parsing code (look for where `»`/`«` / `<<`/`>>` hyper-op delimiters are
  recognized and the wrapped-operator token is parsed) — likely under `src/parser/` in the
  operator/hyper-op handling. Needs to accept a nested hyper-form operator spelling
  (`>>+<<`/`«+»`) as the operator between the outer `»`/`«` pair.

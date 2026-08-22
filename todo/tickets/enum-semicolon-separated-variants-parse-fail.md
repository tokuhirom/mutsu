# `enum Name (a => 1; b => 2;)` — semicolon-separated variant list fails to parse as an enum

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/nativecall.rakudoc:1186`). Despite being discovered via the `nativecall.rakudoc`
example, this is a general parser bug unrelated to NativeCall — the failure happens at
compile time, before any FFI call is attempted.

## Root cause

`enum Name ( pair, pair, ... )`'s body parser
(`parse_enum_decl_body_with_type` in `src/parser/stmt/decl/enum_decl.rs`) tries
`parse_static_enum_variants` (comma-separated pairs) first; if that fails it falls back to
parsing the body as a single "dynamic" expression followed by an optional `,`-separated
list (`expression(r)` then expects `,` or the closing `)`). Neither path accepts `;` as a
variant separator, so `enum Foo (A => 0; B => 10)` fails to parse as an enum body at all —
and because `enum_decl` returns an error, the statement-level parser backtracks and treats
the bare word `enum` as an ordinary (undeclared) function-call statement instead, producing
a confusing `Undeclared routine: enum used` error rather than a body-parse error.

Real Raku accepts `;` as a statement/expression separator inside a parenthesized term in
general (`(a; b; c)` builds a List from each statement's value — see the existing ticket
`paren-statement-list-trailing-empty-element.md` for that general mechanism), and `enum`'s
own body apparently reuses that general grammar rule rather than a comma-only list. mutsu's
`enum` body parser has its own hand-rolled comma-only variant-list logic that never
delegates to (or falls back to) the general semicolon-list parsing path.

## Minimal repro

```raku
enum Foo (A_INET => 0; A_INET6 => 10);
say A_INET;
```

- `raku`: `0`
- `mutsu`:
  ```
  ===SORRY!=== Error while compiling ...
  Undeclared routine:
      enum used
  ------>enum Foo (A_INET => 0; A_INET6 => 10);
         ^
  ```

Multi-line form (as it appears in the doc) fails the same way:

```raku
enum AddrInfo-Family (
    AF_UNSPEC => 0;
    AF_INET   => 2;
    AF_INET6  => 10;
);
```

Comma-separated forms (with or without a trailing comma, with or without surrounding
parens) already work correctly and are unaffected.

## Affected files

- `src/parser/stmt/decl/enum_decl.rs` — `parse_enum_decl_body_with_type`'s `(` branch:
  `parse_static_enum_variants` needs to accept `;` as an alternate separator (in addition to
  `,`), or the "dynamic" fallback's `,`-loop needs a `;`-loop counterpart (or should
  delegate to the same statement-list-to-List parsing used for a plain parenthesized `(a;
  b; c)` term, once `paren-statement-list-trailing-empty-element.md` establishes that
  mechanism is sound).

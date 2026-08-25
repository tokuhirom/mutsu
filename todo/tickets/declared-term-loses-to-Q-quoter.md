# A declared term named `Q` loses to the `Q//` quoting construct

Found while probing enum bodies (`news/2026-08/enum-semicolon-separated-variants-parse-fail.md`)
with a variant literally named `Q`.

## Repro

```raku
enum E <P Q>;
say Q, "x", 2;
```

- `raku`: `Qx2` — `Q` is a *declared* term (the enum value), so it wins over the
  generic `Q` quote language.
- `mutsu`:
  ```
  ===SORRY!=== Error while compiling ...
  Confused. Two terms in a row
  ------>enum E <P Q>; say Q, "x", 2;
  ```

mutsu parses the `Q` as the generic quoter `Q<delim>...<delim>` with `,` as the
delimiter, so `Q, "x",` is read as a quoted string and the trailing `2` becomes a
second term in a row. Note the two-item form `say Q, "x";` happens to *succeed* —
there the mis-parse consumes the whole argument list and no stray term is left —
so the symptom only shows up with three or more items.

## Why it matters / scope

Narrow but real: it is not enum-specific. Any declared symbol spelled `Q`
(an enum value, a `constant`, a sub, a class) should shadow the quote language,
the same way a declared `q`/`qq` would. The fix belongs wherever the parser
decides to enter the quote slang for a bare `Q` — it needs to consult the
declared-symbol registry (`register_user_enum_value` / `register_user_type` and
friends, already used by `parser::stmt::simple`) first.

## Affected files (starting point)

- `src/parser/primary/string/` — the `Q`/`q`/`qq` quote-construct entry point
- `src/parser/stmt/simple.rs` — `is_user_declared_enum_value` and the other
  declared-symbol predicates the check would consult

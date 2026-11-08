# A ratchet keeps parameter placeholder spellings out of the consumers

`check-frontend-debt` now counts the quoted placeholder names (`__subsig__`, `__type_only__`,
`__literal__`, `__type_capture__`) outside `src/ast.rs` and pins the count at 0 (ADR-15761 §2.2);
the last four sites (`value::signature`, the duplicate-parameter check) use the `ast` constants.
The `internal-name-literals` ceiling falls from 38 to 34, which the first slice earned.

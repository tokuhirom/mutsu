# A `use` after `unit role` is visible inside `try` bodies

A routine imported by a `use` that follows a `unit role X;` line resolved in the
role's methods but failed with "Unknown function" inside a `try` body: the
`try` frame lost the enclosing routine's lexical package. Block frames without
a declaring file now inherit it from the enclosing frame (Selkie `t/107`, #15291).

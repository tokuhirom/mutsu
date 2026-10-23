# Parameterized role attribute traits run per composition; same-named sibling roles do not share them

- `role R[::T] { has T $.x is mooish(:lazy) }` never ran the attribute's custom trait. The trait now
  runs for each consuming class with the type parameter bound, and `.^attributes[0].type` reports the
  bound argument (`Bar`) instead of `T`.
- Two sibling scopes each declaring `my role R` with different attribute traits applied the first
  one's meta-object to the second; a changed trait list now starts afresh.

AttrX::Mooish `t/020-role.rakutest`: 2 more subtests pass ("Require method", "Parameterized").

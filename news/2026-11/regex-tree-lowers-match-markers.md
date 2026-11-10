# Regex tree lowers match markers and `<?>` directly

`<(`, `)>` and `<?>` in a regex literal's retained `RegexTree` now lower straight to the matcher plan instead of
re-parsing the regex string (ADR-0088 section 80, #8033).

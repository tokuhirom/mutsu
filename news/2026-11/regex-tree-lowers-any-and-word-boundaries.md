# Regex tree lowers `.` and word boundaries directly

A regex literal's retained `RegexTree` now lowers `.`, `<<` and `>>` straight to the matcher plan
instead of re-parsing the regex string (ADR-0088 section 79, #8033).

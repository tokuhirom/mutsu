unit module ExportedRegexDeclarator;

# A `my token ... is export` (Collection, Number::More) — the Regex becomes
# importable under `&name`.
my token digits is export { \d+ }

# A tagged export is hidden from a plain `use`.
my token wordy is export(:wordy) { \w+ }

# `regex` and `rule` take the same traits.
my regex spaced-out is export { \s* }
my rule two-words is export { \w+ \w+ }

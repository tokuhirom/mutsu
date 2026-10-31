# `:exists??` parses: a colonpair name no longer swallows a trailing `?` or `!`

`%h<a>:exists??'Y'!!'N'` (adverb directly followed by `??`) died with
"Two terms in a row" because the colonpair parser consumed one `?`/`!` after the
name, leaving a lone `?` for the ternary. Raku has no `:name?` colonpair, so the
suffix is no longer consumed. Found via the `fez` distribution
(`Fez::Util::Glob`): all four of its test files now match raku, so `fez` goes
from `blocked_load` to parity (`t/00-use` 23/23, `t/03-glob` 27/27).

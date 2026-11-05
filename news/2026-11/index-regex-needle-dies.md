# `Str.index` / `rindex` refuse a Regex needle

`"xabcx".index(/bc/)` and `rindex(/bc/)` answered `Nil` in mutsu. A `Regex` is not `Cool`, so
rakudo's multi dispatch finds no candidate and dies with `Cannot resolve caller index(Str:D:
Regex:D); none of these signatures matches:` and the candidate list. Both methods now raise the same
`X::Multi::NoMatch` (`src/runtime/methods_string_index.rs`); plain and list needles are unchanged.
Test: `t/regex/str-index-regex-needle.t`.

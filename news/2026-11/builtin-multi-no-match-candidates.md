# Built-in multi methods list their candidates in `X::Multi::NoMatch`

Found by the raku-doc differential sweep (`Type/Seq.rakudoc:208`, `Language/signatures.rakudoc:299`).
`(1..12).Seq.skip(2, 3, 4)` died `No matching candidates for method: skip`, and `Str.lines` printed
the `Cannot resolve caller` header with an empty candidate list. The candidates of rakudo's multi
methods (4623 rows) are now data: `scripts/builtin-method-sigs.raku` reads them off rakudo's dispatchers
into `src/runtime/builtin_method_sigs.tsv`, and `make_multi_no_match_error_detailed` fills an empty
list from it (walking the invocant's builtin ancestry). First slice of #16736: other built-in
no-match sites that build their own message still need to route through it.

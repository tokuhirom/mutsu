# A named parameter with a sub-signature is not applicable when its name is absent

`multi expand-capture(:literal($) ($payload, *@arguments), *%args)` matched a call that did not pass
`:literal` at all, because an absent optional named parameter was never checked against its
sub-signature. In SQL::Abstract that made `expand-capture(|\(:op['like', ...]))` pick the
`:literal` candidate (declared first) instead of the `:op(@) (Str $op where 'like'|'not-like', ...)`
candidate, so `select` died on a type check in `Literal`'s `$!payload`.

Candidate matching now treats an absent, default-less named parameter as unpacking nothing: a
sub-signature (inline or trailing) that needs an element makes the candidate inapplicable, as in
rakudo; one whose parameters are all optional stays applicable.

Found by SQL::Abstract (`t/01-basic.rakutest`, 13 -> 32 results before the next failure).
Test: `t/routines/signature/multi-named-subsignature-absent-not-applicable.t`.

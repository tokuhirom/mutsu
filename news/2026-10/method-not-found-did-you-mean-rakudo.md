# `X::Method::NotFound` suggestions follow Rakudo

The "Did you mean" text of a missing method now comes from the same algorithm Rakudo uses
(`X::Method::NotFound.!create-message`): the candidates are the invocant's whole method set
(the MRO and the composed roles, read from the Rakudo oracle snapshot, so inherited `Any`/`Mu`/`Cool`
methods count), a name is a suggestion when it is within 1/2/3 edits (by name length) or contains a
long name, the list is ordered by distance then name and cut at the fourth entry's distance, several
candidates read `Did you mean any of these: 'a', 'b'?`, the tip joins the sentence, the whole message
is wrapped at 72 columns like `naive-word-wrap`, and a `Str` invocant reads `for string 'abc'`.
`5.lazy` and `"x".lazy` now give Rakudo's text (#11816). The old hand-written per-type candidate lists
and the single-suggestion rule are gone.

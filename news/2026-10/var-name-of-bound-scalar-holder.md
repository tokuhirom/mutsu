# `.VAR` of a scalar bound to an element or another scalar is `Scalar`

`my $r := @a[0]` and `my $t := $hi` bind `$r`/`$t` to an existing `Scalar`
container: the array element, or `$hi`'s own container. rakudo reports
`$r.VAR.^name` as `Scalar` even when that Scalar holds a Hash or an Array.
mutsu reported the held aggregate's type (`Hash`), because the bind recorded
"bound directly to a container" whenever the bound value was an aggregate.

The record is now skipped when the bound value is held by a Scalar: an itemized
holder word, or a cell or value that is itemized (#11111). A scalar bound to an
`@`/`%` variable itself (`my $w := %h`) still reflects `Hash`/`Array`, as in
rakudo.

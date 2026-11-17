# `.raku` of a lazy list itemizes its positional elements, as rakudo's does

Rakudo's `List.raku` renders a lazy list's prefix through
`my @elements = self.head(101)`, so every element is read back out of an
Array's Scalar container: `((1,2),(3,4)).lazy.raku` is
`($(1, 2), $(3, 4)).lazy.Seq`, a hash element renders `${:a(1)}` and a `Nil`
reads as the element default `Any`. mutsu rendered the elements bare, which
was most visible on a lazy `X` product (`(lazy gather {...}) X (3,4)`), whose
rows rakudo shows as `$(1, 3)`. `lazy_seq_raku` now renders each element the
way it reads out of an Array element; what a consumer pulls (`.head`, `for`,
`.flat`) is unchanged. The other half of #16139 — a lazy list stops being
`.is-lazy` once it has been reified to its end — is #17908.

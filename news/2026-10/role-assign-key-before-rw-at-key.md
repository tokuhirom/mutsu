# A role's ASSIGN-KEY runs before an `is rw` AT-KEY takes the assignment

`$d<a> = 4` on an `is Hash` class whose composed role defines both an
`is rw` `AT-KEY` and an `ASSIGN-KEY` assigned through the `AT-KEY`'s return
value and never called `ASSIGN-KEY`. Rakudo's `postcircumfix:<{ }>` calls
`ASSIGN-KEY`, and only the default one assigns through `AT-KEY`. In PDF,
`PDF::COS::Tie::Hash` is such a role, and `$obj<B> = :ind-ref[42, 5]` died with
`X::Assignment::RO: rw method 'AT-KEY' does not expose an assignable attribute`
(#13088). The rw-`AT-KEY` store path now defers to a user `ASSIGN-KEY` /
`ASSIGN-POS`, declared or role-composed.

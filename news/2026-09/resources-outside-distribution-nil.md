# `%?RESOURCES` outside a distribution is `Nil`

`%?RESOURCES` used to evaluate to an empty Hash when no distribution was in
scope, so `%?RESOURCES<x>.lines` died with "No such method 'lines' for
invocant of type 'Any'". Like Rakudo it is now `Nil` there; inside a
distribution (real or installed) it is unchanged. Pinned by
`t/modules/compunit/resources-outside-distribution-is-nil.t` (#9879).

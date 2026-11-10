# An object sub-signature refuses a surplus public attribute

`sub f(Ge (:$left))` called with a `Ge` that also has a public `$.right` died in
raku ("Unexpected named argument 'right' passed in sub-signature") but matched in
mutsu. An object destructured by an all-named sub-signature is now unpacked through
its public attributes like a Capture: one the sub-signature does not name is refused,
in the binder and in multi dispatch, unless a `|` / `*%rest` slurpy takes it.

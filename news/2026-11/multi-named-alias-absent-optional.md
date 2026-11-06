# A renamed named parameter is optional again in multi dispatch

`multi f(:a($u)) {...}` could not be called as `f()`: the #16612 rule "an absent named
parameter whose sub-signature needs an element is not applicable" also fired for
`:a($u)` / `:if-not-exists($x) where ...`, whose one-parameter "sub-signature" only
renames the variable. The rule now skips a rename (`named_alias`); a real sub-signature
(`:literal($) ($payload, *@rest)`) keeps being inapplicable when its name is absent.
Red's `MetamodelX::Red::Model.create-table` (`:if-not-exists($unless-exists) where not
.defined`) could not be resolved because of it, which stopped RedFactory at the first
`red-do`.

# `.origin` of a hyper prefix

The `MetaPrefix::Hyper` of a hyper prefix application (`|<< @x`, `-<< @x`,
`-« @x`) and the `Prefix` it holds now carry the ranges rakudo 2026.11 gives
them. The `MetaPrefix::Hyper` covers the operator and its `<<` or `«`, and
the `Prefix` covers the operator alone (#15780).

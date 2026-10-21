# Two `use`s in a block that import the same multi family bind one family

`{ use Wrapper; use Provider; calendar(1..4, :format<html>) }` died with
`Unexpected named argument 'format' passed` when `Wrapper` extends `Provider`'s
exported proto with a candidate of its own: each `use` filled its own lexical
import set, and a call read only the innermost set that bound the name. A `use`
now starts its family from the candidates the earlier sets of the same scope
bound for the name, so the innermost set carries all of them (#14140).

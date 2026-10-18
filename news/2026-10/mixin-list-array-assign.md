# A role mixed into a list flattens on `@`-assignment

`my @b = (1, 2, 3) but R` held one element, the mixed-in list itself, so `@b.elems` was 1 and
`@b.gist` rendered `[(1 2 3)]`; Rakudo flattens the list into three elements. `coerce_to_array`
now flattens a mixin over an `Array`/`Seq`/`Slip` as it does the bare list (the mix-in is a new
object, so `$[1, 2] but R` flattens too), and `ItemizeVar` keeps a scalar holding one as a single
item (`my $l = (1, 2) but R; my @e = $l` is one element). The wrong element count had been hidden
by the old `gist` gate rendering the nested mixin without parentheses (`roast/S02-types/list.t`).

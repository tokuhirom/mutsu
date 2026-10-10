# `.list`/`.List`/`.Array` keep a role mixed into a scalar object

`$attr does R; $attr.list[0].method` (and `(5 but R).list[0]`) lost the mixin because the
role-mixin method delegation handed `.list` to the bare inner value, whose scalar fallback
answered `[inner]`. The delegation now puts the whole mixin back as the single element when
the inner answered just itself; an aggregate's mixin still folds into its elements.
Closes #12591.

# A method of an `is Hash` class keeps its attribute writes across a `self.` call

Calling a builtin Associative method on `self` inside a method of an `is Hash`
(or `is Map`) class rebuilt the instance from the attribute snapshot taken before
the call, so an attribute hash the method wrote afterwards (`%!entries<y> = 2`
after `self.elems` or `self<k> //= 5`) was dropped. The delegation now updates
the backing storage in the instance's shared attribute cell. PDF's
`Tie::Hash.tie-init` hit it: only the first `is entry` attribute was registered,
so `ok $dict.DictInd.is-indirect` failed in `t/cos-tie-entry.t`.

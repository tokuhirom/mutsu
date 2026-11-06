# An explicit `$self is rw` invocant writes through to the caller's variable

`method viv(C:U $self is rw:) { $self = C.new }` assigned only its own `$self`: the method
binder rebound the plain invocant value over the caller's container it had just bound under
the `$self` lexical key, so `my C $b; $b.viv` left `$b` a type object (BinaryHeap's
auto-vivifying `push`, #16863). The container now stays bound; `self` still names the original
invocant.

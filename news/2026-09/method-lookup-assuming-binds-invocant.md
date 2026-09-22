# A looked-up method can be curried with `.assuming`

`.^lookup` and `.^find_method` return `Method` instances. Their callable
payload already includes the method's invocant parameter, so
`$type.^lookup('method').assuming($object)(...)` must bind that object before
the remaining arguments are supplied.

mutsu exposed the callable payload for direct `CALL-ME`, but left
`.assuming` to the generic `Method` instance fallback. That raised
`No such method 'assuming' for invocant of type 'Method'`, which stopped
`ML::Clustering`'s distance-function lookup before either test file could run.
Method instances now delegate `.assuming` to their existing callable payload.

Pinned by `t/oo/method/classhow-lookup-method-instance-callable.t`, including
the `ML::Clustering` idiom.

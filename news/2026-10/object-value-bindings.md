A scalar name bound directly to an object now records the immutable value
binding on its own binding cell. The object retains its writable attributes,
while assigning a different value through that name is refused.

Container reflection asks the binding cell before the dynamically scoped
readonly registry, so an inner declaration with the same name does not change
the outer binding's `.VAR` identity.

# Materialize live Supplier supplies through completion

`Supply.list` now subscribes to a live Supplier, waits for its `done` event,
and collects values emitted after subscription. Previously the generic
coercion returned the Supply's empty `values` attribute immediately. Live
subscriptions no longer replay values emitted before `.list` began.

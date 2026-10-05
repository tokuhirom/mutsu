# List positional views use method rows

`List.values`, `kv`, `pairs` and `antipairs` now dispatch through the built-in
method table. Arrays inherit the same handlers, while shaped and lazy collection
paths keep their existing behavior.

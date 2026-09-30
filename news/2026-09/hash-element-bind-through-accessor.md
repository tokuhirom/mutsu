# Preserve hash element bindings through accessors

Binding a hash element reached through an object accessor now stores the bound value or shared source container. This lets a callable captured by a parameter remain callable after insertion and keeps later writes to either side of a scalar binding visible to the other. A value bound from an expression remains read-only when reached through the accessor.

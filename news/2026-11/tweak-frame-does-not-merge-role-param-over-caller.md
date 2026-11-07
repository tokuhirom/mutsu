# A class's TWEAK frame no longer merges a composed role's type parameter into the caller

A parametric role's type parameter (`role Pk[Qos $qos = A]`) is bound while an
instance of a class composing it is constructed, and the class's own `TWEAK` /
`BUILD` frame saw it. The frame's exit merged it back over a caller variable
with the same name that had been passed as a named argument (`P.new(:$qos)`), so
the next `.new` received the role parameter's default (Protocol::MQTT
`Packet::Publish`, #16987). The merge now treats the receiver class's role
parameter names as frame-local, as it already did for a role method's own.

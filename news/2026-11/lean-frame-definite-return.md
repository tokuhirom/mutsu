# A planned method declared `--> Nil` runs in the lean frame

The lean method frame refused any method with a return type, so every
`method m(... --> Nil) { ... }` ran on the general fast binder. That is a
common shape: MCP::Server::Tool::Web's extractor declares most of its methods
that way.

A literal definite return (`Nil`, `True`, `False`, `Empty`) names a value,
not a type to resolve. Its finalize therefore reads no capture and no owner
scope, and the lean frame now admits it, finalizing the result exactly as the
fast binder does (#18267). The slot frame, which finalizes without a return
spec, still refuses any return type.

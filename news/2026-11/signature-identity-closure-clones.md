# Closures of one declaration share their `.signature`

Rakudo has one `Signature` object per declaration, which every closure clone
of it shares. mutsu gave each closure its own signature whenever a parameter
carried code that reads the enclosing scope: a `where` clause or a computed
default on a bodiless routine, and every signature literal
(`:($x where * > $l)`) (#18728).

Every routine's signature is now cached per declaration. The signature's
binder, the scope its `ACCEPTS` binds in, points at the closure the signature
was last read from. This matches Rakudo, whose shared signature reads the scope
of the closure cloned last.

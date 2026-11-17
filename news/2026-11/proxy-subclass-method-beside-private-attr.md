# A Proxy subclass's method beside a same-named private attribute is called directly

A method that a `Proxy` subclass declares, called on the Proxy's `.VAR`, is
dispatched straight to its compiled body when nothing else can answer the call.
That fast path also declined any method whose name matched one of the
subclass's attributes, because such an attribute's accessor would answer
instead. Only a *public* attribute has an accessor, though. AttrX::Mooish's
`AttrProxy` declares `has Bool $!is-set` next to `method is-set`, so every
`.VAR.is-set` (four per lazy-attribute round) ran the interpreter's whole
method cascade. Now only a public attribute keeps the call off the fast path.

The gain is small: about 1% of Mooish's per-round instructions (7.97M to 7.88M,
measured with callgrind on a release build). It removes one interpreter
fallback from the hot path of AttrX::Mooish's `t/090`. The rest of that file's
cost (#17335) is spread thinly across the call machinery.

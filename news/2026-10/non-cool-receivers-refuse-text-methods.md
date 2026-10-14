# Non-`Cool` receivers refuse `Cool`'s text methods; a `Cool` subclass is read through its `.Str`

`contains`, `index`, `substr`, `starts-with`, ... are declared on `Str` and `Cool` only. A `Pair`,
`Capture`, `Version`, a quanthash or a code object answered them from its string form; Rakudo
raises `X::Method::NotFound`, and the gate that already refused them for buffers now covers these
receivers (the text and search names only: `fmt` and `split` are `Any`'s). A user class that is
`Cool` and declares its own `Str` is now read through it (`C.new.substr(1)` was `<21>`, the
default instance rendering, instead of the `.Str` text) on the interpreter and the VM paths.
Closes #13035.

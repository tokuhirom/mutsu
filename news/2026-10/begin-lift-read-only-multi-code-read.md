# A nested BEGIN reading a multi through `&name` is lifted

A `BEGIN` inside a routine that read a multi as `&name` was never run at
compile time: it might change the proto object (`&p.add_dispatchee(...)`), and a
change to the lifted copy would be lost. Because a non-liftable BEGIN also halts
every later nested BEGIN, `sub f { proto p(|) {*}; multi p(Int) { 1 }; BEGIN say
&p.candidates.elems }; sub g { BEGIN say "b" }; say "m"` printed only `m`
instead of `1 b m`.

The lift now looks at how each `&name` is used in the AST: a read-only method
(`.candidates`, `.signature`, `.name`, ...) or a call through the code object
is safe and the BEGIN is lifted; anything else (`add_dispatchee`, handing `&p`
on) still keeps it in place (ADR-0134, #13096).

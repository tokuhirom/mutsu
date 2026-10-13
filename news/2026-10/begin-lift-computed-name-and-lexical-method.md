# A nested BEGIN beside a literal computed name or a lexical method is lifted

A routine named by a string literal (`sub ::("helper") { }`), a lexical method
(`my method m { }`) or a method-level proto outside a package (`my proto method
p(|) {*}`) used to keep every nested BEGIN of its scope from running at compile
time, and with it every later nested BEGIN in the unit. `sub f { my method mm()
{ }; BEGIN say "a" }; sub g { BEGIN say "b" }; say "m"` printed only `m`
instead of `a b m`.

Each of them is now copied into the lifted BEGIN the way a `my sub` is
(ADR-0134, #13096).

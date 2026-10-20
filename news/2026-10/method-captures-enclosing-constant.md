# A method keeps its class's enclosing `my constant`

`do { my constant C = 'c'; class K { method c { C } }; K.new }.c` read the
string `C` instead of `c` (#14068). The method compiler did not know about the
`my constant` of the scope declaring its class. It compiled `C` as a by-name
bareword, which resolved through the caller's env and, once that scope had
exited, fell back to the bare name.

The method compiler now gets the enclosing scope's constants (the term-key
slots, `\C`) along with its sigilless bindings. The method then reads them by
name, and its declaration-time capture holds them past the scope's exit.

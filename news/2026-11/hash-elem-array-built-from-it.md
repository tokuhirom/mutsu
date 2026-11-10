# A Hash is an element of the Array built from it, with a slang loaded

`%h ∈ @p` after `my @p = %h, %h` answered False once a `Slangify` slang was
imported. The slang host runs on its own thread, and while a second mutator
thread is alive a set operator reads a snapshot of each shared `Hash`/`Array`
operand; the snapshot got a fresh `.WHICH` id, so the needle was no longer the
element it came from. A read snapshot now adopts the original's id
(`WhichId::adopt`), and `values_identical` compares Hash/Array identity by that
id as well as by node (#17325).

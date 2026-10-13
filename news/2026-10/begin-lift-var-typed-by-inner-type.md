# A nested BEGIN can read a variable typed by a class of its scope

`sub g { my class K { }; my K $v; BEGIN say $v.raku }` now prints `K` at compile
time, as on rakudo, where mutsu used to skip the BEGIN unless `g` was called
(and keep every later nested BEGIN of the unit on the old path with it). The
variable's static cell is declared at the unit's level, where `K` does not
exist, so it is now untyped and starts from `Nil`, which resets the variable to
its type object; a `:D` variable falls back to its nominal type (#10471).

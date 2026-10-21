# An argument-less user trait on a class runs in the declaration phase

`class C is flagged { }` (no argument) is kept by the parser as a parent name
and deferred at run time to the unit's `trait_mod:<is>`. The BEGIN prologue now
recognizes the same case (a lowercase `is NAME` that no declared type answers,
in a unit with its own `trait_mod:<is>`) and places the declaration there, so
the trait runs in the declaration phase like its parenthesized sibling (#14369).

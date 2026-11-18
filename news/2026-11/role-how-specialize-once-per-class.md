# A role's HOW `specialize` runs once, at the class's real declaration

A role whose HOW has a `specialize` override (a meta-role a trait mixed in)
gets it called when a class composes the role. mutsu also called it for the
class's forward-reference shell, the registration that runs before the program
reaches the declaration. So the hook ran twice, and the first time it ran
before the class's own body.

Method::Also showed why that matters. Its `is also<alias>` mixes a `compose`
override into the class HOW, both from the class's method traits and from the
role HOW's `specialize`. Because the shell's `specialize` had already mixed in
the hook, the real declaration saw it as present "before its traits ran" and
never queued the compose. `class Bar does Ber { multi method foo() is also<bar>
{ 42 } }` then had no `bar` method. The shell now drops its queued
specializations, and the hook runs once, at the real declaration. Method::Also's
`t/01-basic.rakutest` is back to 11/11.

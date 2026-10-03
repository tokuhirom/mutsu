# `.^private_methods` and bare private-method names

`Metamodel::PrivateMethodContainer.private_methods` is now implemented: a
class's own private methods and private submethods, as `Method`/`Submethod`
objects in declaration order (including those composed in from roles, and
excluding inherited ones, as in Rakudo). It shares one ordered walk with
`.^private_method_table`.

A private method object's `.name` is now its bare name (`method !z` reports
`z`, matching Rakudo) instead of `!z`, and a `Submethod` object's `.gist` /
`.Str` is its name rather than `Submethod<id>`.

Found by the ecosystem roulette on Manifest::StopWar, whose `t/01.t` walks
`self.^private_methods` and calls each one back through `self!"$name"()`; the
file now passes under mutsu.

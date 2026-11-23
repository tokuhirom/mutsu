# `dd` of a type object no longer prints a `$name = ` prefix

#18985 made `dd` prefix a container argument with its name (`$x = 5`), as
rakudo's `dd` does. But a bareword argument records its own name as its
source, just as a scalar `$x` records `x`. So `dd IO::Path`, or `dd Foo`
for a class, printed `$IO::Path = IO::Path`, while rakudo prints
`IO::Path`. Together with #19055's RakuAST test this turned `main` red
(#19065). A type object passed by its own name now gets no prefix; a scalar
that holds a type object still shows its name (`$t = IO::Path`).

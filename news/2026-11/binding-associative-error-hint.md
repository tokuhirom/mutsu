# Binding a List or Array to a `%` variable carries rakudo's hint

`my %h := (1,2)` and `my Int %z := [1,2]` now say "You have to pass an explicitly
typed hash, ...", word-wrapped as rakudo does, and a typed hash names
`Associative[Int]`. One `container_binding_message` now serves every `:=` bind of
a container variable (Positional and Associative), next to the parameter-binding
hint it shares (#16735).

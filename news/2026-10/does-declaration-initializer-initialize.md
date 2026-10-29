# `my %h does R = ...` passes `:INITIALIZE` to the role's STORE

The initializer of a declaration that mixes a role into a container
(`my %h does R = a => 1`) is now handed to the role's `STORE` with
`:INITIALIZE`, as rakudo does; a later `%h = ...` still passes none. Found with
WriteOnceHash (#15390).

# .^methods lists composed-role methods; Sub.^can('of') finds Block's own

`List.^methods`, `Map.^methods`, `Pair.^methods` and `Hash.^methods` now list the
methods their composed roles (`Positional`, `Associative`) contribute, with the role
as `.package`, as Rakudo does, and `Sub.^can('of')` counts both `Block.of` and the
`Callable` one (#12990).

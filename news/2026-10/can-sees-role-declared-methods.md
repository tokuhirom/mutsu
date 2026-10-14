# `.^can` finds the methods a built-in class gets from a role

Rakudo flattens a role's methods into the class that does it, so `List.^can('of')`,
`Pair.^can('keyof')` and `Map.^can('keyof')` answer a candidate. mutsu's `.^can` read the
recognition catalog along the class MRO only, never visiting the roles a catalog class composes
(`Positional`, `Associative`, `QuantHash`), so those were empty. The MRO walk now also asks the
roles composed at each level (the same `composes_role_at` rule the method table registers rows
with). `Sub.^can('of')` (Rakudo: `Block` and `Callable`) and `Hash.^methods` (four `of`/`keyof`
entries, the duplicates `.^methods` dedups by name) are still short; they stay on #12990.

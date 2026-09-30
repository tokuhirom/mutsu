# Role mixin MRO keeps the wrapped class

The MRO of a value with a role mixed in now includes its wrapped class immediately after the synthetic mixin type. `.^parents`, including `:local`, `:all`, and `:tree`, follows the same ancestry. Role-inclusive MROs retain both the mixed role and the wrapped class, and a renamed mixin type renders with its new name.

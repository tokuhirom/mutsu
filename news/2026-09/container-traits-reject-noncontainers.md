`is` on an `@` or `%` declaration now binds the named class or role as the
container, so a type without the corresponding storage protocol rejects its
initializer instead of silently falling back to Array or Hash. Whole-container
binding also enforces the implicit Positional or Associative constraint.

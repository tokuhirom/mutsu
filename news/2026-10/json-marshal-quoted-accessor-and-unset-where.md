# Quoted method names reach accessors; unset `where` attributes construct

JSON::Marshal's last two failing test files now pass, so all 14 do.

- **A quoted or run-time method name reaches a generated accessor** whose name
  is also a built-in method. `$obj."$n"()` with `$n = "hash"` on a class with
  `has %.hash` ran the built-in `.hash` coercion, which died with "Odd number
  of elements". The plain `$obj.hash` was right all along, because the
  accessor fast lanes answer it. Those lanes are skipped for a quoted name, and
  the native-method gate behind them counted only explicit methods, not
  generated accessors. JSON::Marshal reads every attribute this way.
- **An attribute with a `where` constraint that is left unset** holds its
  undefined type object, and the predicate is no longer run against it.
  `has $.depends where Positional|Associative` constructs with no value given,
  as in Rakudo. A value that is actually given is still checked.

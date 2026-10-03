# Bound JSON nesting depth

`Rakudo::Internals::JSON.from-json` and `.to-json` now raise a catchable error
for excessively nested data instead of exhausting the Rust stack. The limit
also covers the traversal that prepares user-defined associative and positional
values for serialization.

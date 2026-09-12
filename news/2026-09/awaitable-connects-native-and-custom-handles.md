`Promise` and `Channel` now report their `Awaitable` membership from the
builtin type catalog, and `await` invokes `get-await-handle` for user-defined
`Awaitable` objects. This brings native membership introspection and custom
awaitable values in line with Rakudo.

# `await` on a module-scope Promise lexical

`await $promise` where `$promise` is a module-scope `my` variable read from a routine or `END`
block of that module died with `No such method 'get-await-handle' for invocant of type 'Promise'`:
the argument arrived as a container reference and was mistaken for a user-defined `Awaitable`.
`await` now reads through the container first. Found via the `Green` ecosystem distribution, whose
`t/01-time.t` and `t/02-concise.t` now pass.

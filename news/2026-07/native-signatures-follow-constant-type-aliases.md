# Native signatures follow `constant` type aliases

A C binding routinely spells its platform types as constants.
`DBDish::mysql::Native` opens with

```raku
constant my_bool = int8;
```

and returns `my_bool` from most of the `MYSQL_STMT` surface —
`mysql_stmt_free_result`, `mysql_stmt_reset`, `mysql_stmt_close`,
`mysql_stmt_bind_result`.

mutsu mapped a signature's type name straight to a C type, so `my_bool` was
unmappable. That path is not an error: an unmarshallable type deliberately
*skips* native registration so the failure surfaces at the call rather than as
a silently mis-marshalled argument. What actually surfaced was misleading — the
declaration kept its stub `{ * }` body, so the method was simply not there:

```
No such method 'mysql_stmt_free_result' for invocant of type 'MYSQL_STMT'
```

even though `MYSQL_STMT.^can('mysql_stmt_free_result')` said yes, and the
sibling methods that return `int32` worked. Declared in a single file rather
than a module the same declaration got far enough to return the `int8` type
object instead of the C result.

Registration now follows the alias to the type it names (bounded to a short
chain) before mapping, for parameters, return types and `CArray[T]` element
types alike. The constant holds the aliased *type object*, so the name is read
back out of the environment.

This was the blocker immediately after
[`self` became lexical in blocks](self-is-lexical-in-blocks.md): with both,
mutsu runs a prepared `INSERT` against MariaDB and reaches result fetching.

Pinned by `t/nativecall-constant-type-alias.t` (libc only, so CI-safe), which
covers an aliased return type on a native method, an aliased parameter on a
plain sub, and a two-link alias chain.

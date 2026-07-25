# Assigning through `$.attr` reports `$.attr` instead of `$!attr` in the type-check error

raku always names the *attribute* in an `X::TypeCheck::Assignment` raised by an
`is rw` accessor assignment, so the message says `$!method` whichever syntax was
used to write it. mutsu echoes the syntax at the assignment site.

## Repro

```raku
class Foo {
    has Int $.n is rw;
    method set($v) { $.n = $v }
}
try { Foo.new.set("s") };
say $!.message;
```

```
raku:   Type check failed in assignment to $!n; expected Int but got Str ("s")
mutsu:  Type check failed in assignment to $.n; expected Int but got Str ("s")
```

## Root cause

The `$.attr` lvalue path passes the source-level name (`.n`) to
`type_check_assignment_typed_error`, which renders it verbatim through
`format_var_name_for_error` (`src/runtime/utils/errors.rs`). The `$!attr` path
passes the twigil form and comes out right. The accessor path needs to normalize
`.name` to `!name` before building the error.

## Impact

Cosmetic today — `HTTP::UserAgent`'s `t/040-request.rakutest` matches only on the
`expected … but got …` tail, so it passes either way. Any roast assertion that
pins the full message on an accessor assignment would see the difference.

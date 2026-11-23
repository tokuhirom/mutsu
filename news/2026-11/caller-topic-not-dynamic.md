# `CALLER::<$_>` reads and assignments die, as `$_` is not dynamic

From language revision 6.d on, the topic `$_` is not a dynamic variable. A
callee therefore cannot read or assign its caller's `$_` through `CALLER::`:

```raku
sub f { CALLER::<$_> }
for 5 { f() }    # Cannot access '$_' through CALLER, because it is not declared as dynamic
```

mutsu returned the caller's topic (#18988). The `CALLER::<$_>` and
`$CALLER::_` reads, and assignments through either form, now throw
`X::Caller::NotDynamic` as rakudo does. A bind (`CALLER::<$_> := $v`,
`$CALLER::_ := $v`) is still allowed and still reaches the caller. Under
`use v6.c`, where `$_` is dynamic, the reads keep working, and so do
`CALLERS::` lookups and `$/` and `$!` access through `CALLER::`.

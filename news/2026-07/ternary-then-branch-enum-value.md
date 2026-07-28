# An enum value can stand in a ternary's then-branch

`COND ?? SOME_ENUM_VALUE !! OTHER` used to fail to parse whenever the parser
reached it in `ExprMode::Full`:

```raku
enum E <FOO BAR>;
my $o;
given $o { .x = 1 ?? FOO !! 2 }
# was: Parse error: Unexpected block in infix position
#      (missing statement control word before the expression?)
```

A bare identifier in then-position is rejected unless the parser can tell it is a
*complete* term rather than the head of a listop call that gobbled the `!!` —
raku throws `X::Syntax::ConditionalOperator::SecondPartGobbled` for
`1 ?? rt123115 !! 3`, and `roast/S03-operators/ternary.t` pins it. The guard's
allow-list covered type names, builtin enum values, builtin constant terms,
user-declared types and sigilless terms. **User-declared enum values were in
none of them**, so every one of them looked like a listop head.

An enum value takes no arguments and so can never be one. The parser now records
the value names of each `enum` it parses (`register_user_enum_value`), and the
guard consults them — the user-declared twin of the `is_builtin_enum_value` it
already asked. The guard itself is unchanged, so the `rt123115` case keeps
failing exactly as it should.

Values of an enum a `use`d module exports are registered too: the module-export
harvest already collected a module's *type* names into the importer's scope
(that is what makes `when X::Foo {}` parse in a file that `use`s the declaring
module), and an enum's values now travel the same way, including through an
intermediate module.

The failure looked arbitrary because it is mode-dependent: `my $y = 1 ?? FOO !! BAR`
parsed (a declaration RHS is read in a non-`Full` mode, where the guard is
skipped) while `$o.x = 1 ?? FOO !! BAR` did not (the method-lvalue statement
path goes through `parse_assign_expr_or_comma` → `parse_comma_or_expr`, i.e.
`Full`). Both guards — `ternary_mode` and its `list_infix_top` twin — are fixed.

## `DBIish` `01-basic`: 3 failed of 30 run → 1 failed of 35

This was the last thing between the `mysql` driver and raku parity, after
[ADR-0015 P2](buf-repr-body-and-native-storage.md) gave `Blob` a real REPR body.
`DBDish::mysql::StatementHandle` has

```raku
.buffer_type = @!column-type[$col] ~~ Blob
        ?? MYSQL_TYPE_BLOB !! MYSQL_TYPE_STRING;
```

where both names are values of `enum mysql-field-type is export (…)` in
`DBDish::mysql::Native`, so the whole module failed to parse and
`DBIish.install-driver('mysql')` died. It installs now, and the file reaches its
full plan instead of aborting five tests short.

The one assertion that still fails is unrelated to either fix:
`$installed>>.key.sort` returns a nested one-element list, because a hyper method
call on an **itemized** list treats it as a single element instead of descending
(`$(:a(1), :b(2))>>.key` is `($("a", "b"),)` where raku gives `("a", "b")`).
Recorded as
[todo/tickets/hyper-method-call-on-itemized-list.md](../../todo/tickets/hyper-method-call-on-itemized-list.md).

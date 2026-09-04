# A `:=` alias of an *undefined* scalar writes through — and is type-checked

A sigilless `:=` alias of a scalar is the source's own container, so a write
through it lands on the source and is checked against the source's declared
type. mutsu did that only when the source already held a value. When it did
not, every write through the alias died:

```
$ raku  -e 'my $a; my \x := $a; x = 5; say $a'
5
$ mutsu -e '...same...'                                    # before
Cannot modify an immutable Package ((Any))

$ raku  -e 'my Int $c; my \x := $c; x = "str"'
Type check failed in assignment to $c; expected Int but got Str ("str")
$ mutsu -e '...same...'                                    # before
Cannot modify an immutable Package ((Int))
```

The failure is not really about typing: an *uninitialized* scalar holds its
**type object** (`my $a` holds `Any`, `my Int $c` holds `Int`), and that is what
the alias was binding. So it hit an untyped `my $a` just as hard, and a
class-typed `my C $o` too.

## Root cause

`exec_set_local_op_inner`'s bind path decides whether to promote the source into
a shared `ContainerRef` cell, and its `val_is_simple_scalar` predicate excluded
`ValueView::Package(_)` along with `Array`/`Hash`/`Sub`/`Instance`. A type object
is not a "simple scalar" in general — binding a type object *literal*
(`my \x := Int`) really must stay immutable — but that exclusion sat inside the
`if let Some(source_name) = bind_source` branch, which runs **only** when the
bind's source is a variable. There, the value being a type object says nothing
about the binding: it says the source is merely undefined.

Without the promotion the alias held the bare `Int`/`Any`, so the write was
refused as a store into an immutable package — and, being refused, never
reached the container chokepoint where ADR-0042's `of`-type check runs. That is
why the typed spelling reported `X::Assignment::RO` where raku reports
`X::TypeCheck::Assignment`: the check was not wrong, it was unreachable.

## The fix

Drop `Package` from that one predicate. The bind then promotes as it does for a
defined source, tagging the fresh cell with the source variable's `of`-type
(`register_container_constraint_named`), and everything downstream follows:

| | before | after |
| --- | --- | --- |
| `my $a; my \x := $a; x = 5` | dies | `$a` is `5` |
| `my Int $c; my \x := $c; x = "str"` | `X::Assignment::RO` | `X::TypeCheck::Assignment` |
| `subset S of Int where * < 128; my S $d; my \y := $d; y = 1000` | `X::Assignment::RO` | `X::TypeCheck::Assignment` |
| `my C $o; my \x := $o; x = C.new` | dies | `$o` is a `C` |
| `my Int $a; my \x := $a; my &blk = sub { x = "s" }; blk()` | dies (RO) | `X::TypeCheck::Assignment` |
| `my \x := Int; x = 5` | dies | dies (unchanged — no source variable) |

The last row is the bound the change has to respect, and it is respected
structurally rather than by a special case: a type object literal has no source
variable, so `bind_source` is `None` and it never reaches this branch.

## Pins

Two subtests in `t/sigilless-alias-typecheck.t`, dual-oracled against raku: one
for the write-through (untyped, class-typed, and the immutable-literal
negative), one for the type check (`Int` and a `subset`).

## What is still open

The **loop-parameter** spelling of the same thing is unfixed, and it is what the
named consumer (`Native::Overflow`'s `t/01-basic.rakutest`) uses:

```
subset SmallInt of Int where -128 <= $_ <= 127;
my SmallInt $a;
for $a -> \x { x = 1000 }      # raku throws; mutsu silently sets $a to 1000
```

A loop parameter over a scalar source does not bind the container at all: it
binds the item and relies on a source-variable writeback
(`store_loop_source_var`) that writes `env` and the local slot directly, with no
constraint lookup and no chokepoint. Rewritten as
`todo/tickets/for-loop-sigilless-param-writeback-skips-the-type-check.md`, which
records the two candidate fixes and why the right one is an ADR-0045 slice
(make the loop parameter a real alias and delete the writeback) rather than
threading `Result` through eleven writeback call sites.

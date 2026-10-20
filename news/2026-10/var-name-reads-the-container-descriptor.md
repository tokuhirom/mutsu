# `.VAR.name` reads the container's own name, not a per-call record

Rakudo keeps a container's name on its descriptor. So `.VAR.name` reports the
declaration that made the container, however many parameters it has been
passed through, and `element` for a container no declaration named. mutsu
stamped that name on the container too. It also recorded, on every call, which
caller variable a `@`/`%` parameter was bound from, in a
`__mutsu_var_source_name::<param>` env key, and `.VAR` preferred that record.

The record named the wrong thing wherever the variable and its container
differ:

```raku
my @b := [1, 2]; say @b.VAR.name;                      # element (was @b)
sub f(@kh) { @kh.VAR.name }; sub h(@kh) { f(@kh) };
say h([1]);                                            # element (was @kh)
```

The record is gone, along with `MetaNs::VarSourceName`, its shadow-metadata
prefix, and the compile-time flag that gated writing it. One more slice of
ADR-0097's retirement of name-derived binding metadata (#8087). The cases the
record still covered now name their containers instead:

- `state @s` names its container, as `my @s` does.
- A `my $*OUT` declaration's cell carries its name.
- `f(my $*OUT)` binds an `is rw` or sigilless parameter through that same
  cell, as it does for a lexical.
- An attribute's container is named after the attribute.

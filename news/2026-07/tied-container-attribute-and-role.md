# A container tie survives a whole-value assignment to an attribute, and a tie can be named by a role

`my %h is Foo` ties the variable to a `Foo` instance: `%h = …` dispatches
`Foo.STORE` instead of replacing the variable with a plain `Hash`. Two gaps kept
that from working in the shape `DBDish::mysql::Connection` uses,

```raku
has %.Converter is DBDish::TypeConverter;
submethod BUILD(…) { %!Converter = method (--> DateTime) { … } }
```

so `my %Converter := $!parent.Converter; %Converter.convert-function($type)`
inside `DBDish::StatementHandle`'s `_row` died with *No such method
'convert-function' for invocant of type 'Hash'* — the tie had been overwritten.

**An attribute's local slot is only seeded by a read.** The tie was detected by
looking in the local slot and in `env`, and an attribute is in neither until
something reads it. So `%!Converter = …` in BUILD saw an empty slot, concluded
there was no tie, and stored a plain `Hash` over the seeded `TypeConverter`. The
symptom read as intermittent: inserting *any* read of `%!Converter` before the
assignment — even `note %!Converter.^name` — made the tie reappear, because the
read seeded the slot the assignment then found. The resolution order now falls
back to `self`'s attribute cell, which is the store of record, and the bound
result is written back into the cell as well as the slot and `env`.

**A tie named by a role was skipped entirely.** raku puns a role used as a
container type, and mutsu represents a punned role as a `Mixin` wrapping an
instance — but the tie gate and the `STORE` re-assignment path both matched
`Instance` only, and the method probes behind them (`has_user_method`,
`class_has_method`) walk the class MRO, which never reaches a bare role's own
methods. So `my %h is TinyAssoc` left a plain `Hash`, and a role-typed attribute
lost its role on the first assignment. The tie now unwraps a `Mixin` to find the
type name, and the `STORE`/`AT-KEY` probes fall back to the role registry
(`has_user_method_including_role`). Punning itself needed no new code —
`Value::package(name).new` already takes the path a role-typed *attribute*
(`has %.C is <Role>`) was using.

An ordinary `%` attribute is untouched: the fallback only fires for a value that
is instance-like *and* passes the existing tie test (a user `STORE` plus a
composed `Associative`/`Positional`).

Pinned by `t/tied-container-attribute-and-role.t`, which checks all four shapes
against raku: a role-tied lexical, a class-typed attribute assigned whole in
BUILD and again in a method, a role-typed attribute bound to a `%` variable and
used through a role method, and a plain `%` attribute staying a `Hash`.

# A role's `.new` instance reports the wrong `.HOW` metaclass

Discovered via the doc-diff harness on `raku-doc/doc/Language/typesystem.rakudoc` (around line
594).

## Repro

```
role R { method m { say 1 } };
say R.new.^mro[0].HOW.^name;
```

- raku: `Perl6::Metamodel::ClassHOW`
- mutsu: `Perl6::Metamodel::ParametricRoleGroupHOW`

## Root cause guess

Calling `.new` directly on a bare role (`R.new`) implicitly creates an anonymous class that does
the role (Raku allows this as sugar), and that anonymous class should have the ordinary
`ClassHOW` metaclass like any other class. mutsu's `.new`-on-a-role path likely returns/reuses
the role's own `ParametricRoleGroupHOW`-tagged type object directly instead of synthesizing (or
tagging) a proper anonymous class wrapper.

## Affected files (starting point)

- `src/runtime/class.rs` — role `.new` handling, `ParametricRoleGroupHOW`/`ClassHOW` tagging

## Suggested next step

Check what mutsu's `R.new` actually constructs today (an `Instance` of `R` directly, vs. an
anonymous class doing `R`) via `--dump-ast`/`.^name`/`.^mro`, and compare to how an explicit
`class C does R {}; C.new` already produces the correct `ClassHOW` — the fix is likely to route
bare-role `.new` through the same anonymous-class synthesis.

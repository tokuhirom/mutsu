# Built-in type hierarchy roles can be composed by user code

`QuantHash`, `Stringy`, `Systemic`, `Baggy`, `PositionalBindFailover` and
`Blob[T]` are roles in Raku's built-in type hierarchy. mutsu already modeled
their native behavior, but did not consistently recognize them as roles when a
user declared a role with `does`:

```raku
role R does QuantHash { }
role S does Blob[uint8] { }
class C does Systemic { }
```

Those declarations now compile. The shared built-in-role registry is used by
role declaration, class composition, role type checking and the role-parent
walk. Native parent relationships such as `Baggy` → `QuantHash` and
`Blob[T]` → `Stringy` are included as well, so a user role that composes one of
these roles retains the expected type relationships and its methods compose
onto consuming classes.

The built-in `Compiler`, `Distro`, `Kernel`, `Raku` and `VM` classes now record
their `Systemic` composition, making `$*DISTRO ~~ Systemic` agree with Rakudo.

The behavior is pinned by
`t/issue-7780-builtin-type-hierarchy-roles.t`.

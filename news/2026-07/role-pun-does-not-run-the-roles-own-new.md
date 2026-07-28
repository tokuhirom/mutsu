# Punning a role no longer runs the role's own `new`

Calling a method on a role type object puns the role into a class and runs the
method there. mutsu punned by *constructing*: it called `dispatch_new` on the
role and dispatched to the resulting instance — which means it called the role's
own `new`, **with no arguments**.

A role whose `new` takes a required parameter therefore lost the arguments of
every *other* method called on the pun:

```raku
role C {
    method new(Int $size) { self.bless }
    method other(Int $size) { "other:$size" }
}
say C.other(3);   # was: Too few positionals passed; expected 1 argument but got 0
```

`NativeHelpers::CStruct`'s `LinearArray[::T]` is exactly that shape — a
parameterised role with `method new(::?CLASS:U: Int $size)` and an
`@!cache handles <AT-POS elems shape>` delegation — so `LinearArray[MYSQL_BIND]`
could not be measured or indexed at all:

```
No such method 'elems' for invocant of type 'NativeHelpers::CStruct::LinearArray'
```

That is where `DBDish::mysql`'s `prepare` sets up its parameter binds.

A role that declares its own `new` now puns to a class and re-dispatches on that
class's **type object**, which is what raku does in every case. Punning
registers a class under the same name, so the retry falls through to ordinary
class dispatch rather than re-entering the role branch; the parameterised branch
goes through `ensure_parametric_role_pun_class` the same way.

Roles that do *not* declare `new` still pun by constructing. Finishing the job —
so that reading an instance attribute through a pun errors like raku instead of
inventing a value — needs two more fixes, both found by
`roast/S13-overloading/typecasting-long.t` and recorded with their repro in
[`todo/tickets/role-pun-should-not-construct.md`](../../todo/tickets/role-pun-should-not-construct.md):
a composed role's methods get copied into the pun twice, and a *type object*
does not match a `::?ROLE:U:` invocant constraint the way an instance does.

Pinned by `t/role-pun-dispatches-on-type-object.t`.

# A role body's guard now rejects the parameterisation it refuses

A parameterised role's body runs once per concrete parameterisation, and a body
statement that dies is how a role refuses a type argument:

```raku
role Guarded[::T] {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    method describe() { "guarded:{T.^name}" }
}
say Guarded[Ordinary].describe;   # raku: dies
```

mutsu accepted it and printed `guarded:Ordinary`. The accepting half already
worked — `Guarded[SomeCStruct].describe` composed and ran the body — so the body
was being evaluated; what went missing was the error on the way out.

## Root cause

Punning a parameterised role (`R[T].new`, or any method call on `R[T]`) goes
through `ensure_parametric_role_pun_class`, which builds a real class composing
`does R[T]`. Composition is what binds the type parameters and runs the role's
deferred body statements, so the `die` did fire — but the function returned
`Option<String>` and mapped every failure to `None` (`.ok().flatten()` on the
candidate lookup, `.ok()?` on `register_class_decl`). The callers read that
`None` as "these type arguments match no candidate" and fell back to
`ensure_role_punned_to_class`, which copies the role's attributes and methods
into a class shell and never runs the body at all. The refusal was thrown away
twice over, and construction succeeded.

## Fix

`ensure_parametric_role_pun_class` now returns `Result<Option<String>,
RuntimeError>`, keeping `Ok(None)` for the genuine "no matching candidate /
arguments do not survive the name round trip" cases that the fallback path
exists for, and propagating an error raised by the composition itself. Both call
sites (`dispatch_new` and the parametric-role method dispatch) propagate it.

The failure is also reported the way Rakudo reports it. A role body statement
that dies during composition is now wrapped in `X::Role::Instantiation` carrying
`.role` (the role type object) and `.exception` (the original), with the message

```
Could not instantiate role 'Guarded' because it died with X::AdHoc; exception details:

    Need a CStruct
```

The wrap happens at the composition site, so `class C does Guarded[Ordinary] { }`
— which already propagated the raw error — gets the same diagnostic. The
composition borrows the caller's topic while running the body, and the error path
now restores it (and the current package) before unwinding.

Naming a parameterisation still does not instantiate it: `my $r =
Guarded[Ordinary]` lives, exactly as in Rakudo, and only punning or composing it
runs the guard.

Pinned by `t/role-body-guard-parameterisation.t` (12 assertions, verified to pass
under `raku` as well as mutsu).

## Not covered

Mixing a parameterised role into a value (`5 but Guarded[Ordinary]`) still does
not run the guard. That path (`compose_role_on_value`) never executes a role's
body statements at all, for parametric and non-parametric roles alike, so it is a
distinct and wider gap; it is recorded in
`todo/tickets/role-body-not-run-on-value-mixin.md`. Relatedly, a non-parametric
role's body currently runs at *declaration* time in mutsu and again at
composition, where Rakudo runs it only at composition — noted in the same ticket.

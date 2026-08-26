# A role parameter's default is evaluated at composition, so a failing default rejects it

```raku
role R[$p = fail("boom")] { };
my $i = 1 does R;
```

silently produced `Int+{R}` instead of throwing. raku rejects the composition with
`X::Role::Instantiation` wrapping the `X::AdHoc` from `fail`.

## Root cause — the defaults were never evaluated on the mixin path at all

The ticket guessed that a `fail`-producing default specifically failed to propagate. The
control case proved something broader: `role A[$p = 5] { method p { $p } }` composed with
`1 does A` bound `$p` to `Nil`, not `5`. mutsu had three separate composition paths and
only two of them instantiated a defaulted parameterisation:

- the class-header path (`class C does A { }`) → `resolve_role_candidate`, which binds
  defaults — correct;
- `.new` on the role (`A.new`) → `materialize_default_parametric_role` — correct;
- the `does`/`but` **mixin operator** → `compose_role_on_value`, which only ever recorded
  `__mutsu_role_param__` markers when explicit `role_args` were supplied, and otherwise
  left every type parameter unbound.

Because the defaults were never evaluated on that path, a default that *raises* had
nothing to raise from.

A second, independent gap sat on the class-header path: `resolve_role_candidate` decides
which candidate matches by attempting the argument binding and discarding any candidate
whose binding returns `Err`. A default expression that dies is exactly such an `Err`, so
the only candidate was discarded and the user saw
`X::Role::Parametric::NoSuchCandidate` — "no matching candidate" — instead of the real
cause.

## Fix

`compose_role_on_value` now instantiates an unparameterised composition at its defaults
via a new `role_default_type_param_bindings`, which runs the real signature binder over
the candidate's `type_param_defs` with no arguments and records the resulting
`__mutsu_role_param__` markers. A defaulted TYPE capture (`role E[::T = Int]`) is read
back under its bare capture name, the same lookup `materialize_default_parametric_role`
performs. The composed *name* deliberately stays the unparameterised `Int+{R}` — verified
against raku, which reports `Int+{A}` with `$p` bound to `5`.

An error escaping that binding is wrapped as `X::Role::Instantiation`, including a
`Control::Fail` (raku reports a `fail` in a default exactly like a `die`); any other
control signal travels on untouched.

`resolve_role_candidate` now remembers the first binding error from a candidate that was
called with NO arguments and whose parameters are all defaulted — the only shape where a
binding failure can come from evaluating a default rather than from a mismatched
argument — and reports it as `X::Role::Instantiation` instead of swallowing it as
"no such candidate".

Pinned by `t/role-composition-gaps.t`.

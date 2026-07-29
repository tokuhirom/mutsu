# A role body's guard does not run on the rejecting parameterisation

A role body statement that rejects a type argument is not executed, so a bad
parameterisation is accepted:

```raku
class Ordinary { has $.x }
role Guarded[::T] {
    die "Need a CStruct" unless T.REPR eq 'CStruct';
    method describe() { "guarded:{T.^name}" }
}
say Guarded[Ordinary].describe;
# raku:  dies with "Need a CStruct"
# mutsu: guarded:Ordinary
```

The *accepting* case works — `Guarded[SomeCStruct].describe` runs the body and
returns normally — so the body is evaluated at least once; what does not happen
is that the `die` propagates out of the parameterisation that should be
rejected. (Rakudo runs the body per concrete parameterisation, at composition
time.)

Found while fixing `.REPR` on a type object
([news](../../news/2026-07/repr-reports-the-declared-representation.md)):
`NativeHelpers::CStruct`'s `LinearArray[::T]` opens with exactly this guard, and
until `.REPR` was fixed the guard fired for *every* parameterisation and silently
killed the role's parameterisation. Now the guard passes for a real CStruct,
which is what the module needs; the rejecting path is the remaining half.

Pin candidate: extend `t/repr-reports-declared-representation.t`, which has the
assertion written out in a comment.

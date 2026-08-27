# A `my role` declared inside `EVAL` leaks out and shadows a later same-named lexical role

A parametric role declared with `my role` inside an `EVAL`'d string is
registered in the outer (process-wide) role registry. A *later*, lexically
scoped `my role` of the same name in the enclosing program then resolves to the
EVAL's version, so its own methods are not found.

## Repro (no Test module)

```raku
try EVAL 'my role R1[::T] { }; my R1 of Str $x = R1[Int].new;';
say "eval done";
{
    my role R1[::T] { method x { T } }
    say "direct: ", R1[Int].new.x.^name;
}
```

```
raku                    mutsu
eval done               eval done
direct: Int             No such method 'x' for invocant of type 'R1[Int]'
```

The EVAL'd `R1` has no methods; the later lexical `R1` has `method x`. mutsu
resolves `R1[Int].new` to the EVAL's method-less registration, so `.x` is
missing. Note the tell: if the EVAL'd role happens to declare the *same*
methods, the leak is invisible — an earlier attempt at this repro used a
matching `method x { T }` on both sides and appeared to pass. The two
declarations must differ for the bug to show.

## How it surfaces

`t/parametric-role-of-type.t` aborts at line 34 under `MUTSU_REAL_TEST=1` with
`No such method 'x' for invocant of type 'R1[Int]'` (runs 5 of 14). It only
shows under the real `Test.rakumod` because that module's `throws-like` really
does `EVAL` the code string it is given, whereas mutsu's native provider
evaluates it by a route that does not register the role the same way. Minimal
Test-based form:

```raku
use Test;
plan 2;
throws-like 'my role R1[::T] { }; my R1 of Str $x = R1[Int].new;',
    X::TypeCheck::Assignment, 'assignment enforced';
{
    my role R1[::T] { method x { T } }
    isa-ok R1[Int].new.x, Int, "direct role instantiation";
}
```

raku passes both; mutsu passes test 1 and aborts on test 2.

## Where to look

Role registration (`src/runtime/registration.rs` / the role registry the
`RegisterRole` opcode writes) is keyed by short name and is not scoped to the
compilation unit, so a re-entrant `EVAL` compile writes into the same table the
outer program reads. The lexical-`my`-vs-package distinction for roles is the
thing that is missing; a `my role` should be visible only in its declaring
scope, and an `EVAL`'s declarations should not outlive the EVAL.

Related, already-known surface: `todo`/`news` records a "lexical class with
reused short name sets up suppression" mechanism (tests 13/14 of
`t/parametric-role-of-type.t`), which suggests short-name suppression already
exists for classes and may just need the EVAL boundary added, or the same
treatment extended to roles.

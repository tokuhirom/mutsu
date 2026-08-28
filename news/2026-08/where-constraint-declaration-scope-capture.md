# A `where` constraint now closes over its declaration scope, not its caller's

A parameter's `where` constraint and its default value are the only two
expressions in a Raku signature that are *evaluated at call time* but *scoped at
declaration time*. mutsu got the first half right and the second half wrong: both
were evaluated against whatever frame happened to be calling the routine, so a
routine that escaped its declaring scope checked its arguments against a
completely unrelated binding.

## What rakudo actually does

```raku
my $a = 1;
{
    my $a = 3;
    sub producer {
        my $a = 2;
        sub bar($x where $a) { $x }
    }
    my &bar := producer();
    bar(2);   # lives  -- $a is producer's 2
    bar(1);   # dies
    bar(3);   # dies   -- the enclosing block's 3 is NOT in scope
}
```

mutsu produced the exact mirror image — `bar(2)` died and `bar(3)` lived —
because `where $a` resolved to the *enclosing block's* `$a = 3`, the binding
visible at the call site. The failing value being precisely `3` is what made the
diagnosis unambiguous: this was not a missing lookup, it was a lookup in the
wrong frame.

Narrowing it with a set of probe shapes showed the damage was wider than the one
roast assertion. With the constraint moved into an anonymous sub returned from a
factory (`sub f { my $f = 2; return sub ($x where $f) { $x } }`), *no* value
passed at all: `$f` was not merely shadowed, it was invisible. The same held for
a parameter **default** (`sub ($x = $f)`), and for a `where` closing over the
factory's own parameter.

## Two independent root causes

**1. The compiler never recorded the signature's declaration-time reads as
captures.** `CompiledCode::compute_free_vars` is a pure scan of a body's
*opcodes*. A `where` clause and a default value are not compiled into the body at
all — they are kept on the `ParamDef` AST and interpreted by
`bind_function_args_values` at call time — so neither ever appeared in
`free_var_syms`. `capture_closure_env` keeps exactly the free variables plus the
system names and drops every other plain user lexical, so the name was pruned out
of the closure's captured env and the call-time evaluation fell through to the
caller's env. Lexical scoping silently degrading into dynamic scoping.

The machinery to harvest those names already existed —
`decl_time_param_free_var_syms` compiles each declaration-time expression as a
throwaway analysis chunk — but it was wired only into the *method* path, and only
to bubble the names *up* into the enclosing frame's capture set, never into the
routine's own. The fix adds `Compiler::fold_decl_time_param_captures`, which
folds those names into the compiled body's own `free_var_syms` right after
`compute_needs_env_sync` (which is what runs `compute_free_vars`) and before
`compute_upvalues` and the parent-slot bake consume the set. It is called from
both `compile_closure_body_with_routine_flag` (which also serves every method
body) and `compile_sub_body_with_deprecation`. A name the routine declares itself
— a later parameter constraining on an earlier one, `sub f($a, $b where $a)` — is
excluded, since the binder has already bound it into the callee env by then.

**2. A named sub escaping as its declaring routine's return value captured a
flattened env that could not see the routine's own locals.** Raku returns the
`Sub` when a `sub` declaration is a routine's last statement, and
`call_compiled_function_named_inner` builds that code object with
`self.clone_env()`. But a `my $a` inside a routine body lives in the frame's
local *slot* — mutsu's dual store does not necessarily mirror it into `env` — so
the flattened snapshot either lacked the name entirely or, under shadowing, still
carried an enclosing scope's value for it. This was not specific to `where` at
all: `sub p { my $k = 2; sub b() { $k } }` returned a sub that read `3` from the
caller instead of its own `2`.

The fix mirrors what `capture_closure_env` already does for an anonymous closure.
A new `Interpreter::inject_frame_locals_for_free_vars` overwrites each of the
escaping routine's free variables with the live value of the declaring frame's
local slot (resolved through the same `resolve_capture_slot` the closure paths
use, so shadow slots are handled identically), skipping anything that is not a
plain user lexical — dynamics, the topic and `__mutsu_*` metadata are resolved
against the live frame by design and must never be frozen. Because the declaring
frame is *gone* by the time such a sub can be called, its snapshot can never go
stale, so the injected names are also recorded as
`SubData::authoritative_captures` (via a new `make_sub_for_routine_owning`): the
call-time env merge is don't-overwrite by default, and without the vouch a
same-named lexical in the calling frame would still win.

## Results

The two roast files this frees, both previously green under the native `Test`
provider and red under `MUTSU_REAL_TEST=1`, now pass under **both**:

| file | test | before (real Test) | after |
| --- | --- | --- | --- |
| `roast/S02-types/subset-6e.t` | 39, `where-constraint picks up the right lexical (+)` | FAIL | PASS |
| `roast/6.c/S02-types/subset-6c.t` | 38, same assertion | FAIL | PASS |

Pinned by `t/where-constraint-lexical-scope.t` (23 assertions, green under real
`raku` as well as under mutsu): the escaping named sub, its body-free-variable
twin, the parameter-default twin, escaping anonymous subs and pointy blocks, two
sibling closures from one factory each keeping their own captured value,
two-deep shadowing, a `subset`'s `where`, a method parameter's `where`, and a
later parameter constraining on an earlier one.

Verified with `make test` green, the full 1436-file whitelisted roast suite green
on a release build, and `scripts/battery-testsuite.sh` green (273/297, the
whitelisted gate) — the last of those because the change touches closure-capture
representation, which `make test` and roast between them have historically
missed.

## Still open

A `class` declared *inside a routine* does not capture that routine's locals at
all — `sub p { my $m = 2; class C { method go() { $m } }; C.new }` reads `Nil`
where rakudo reads `2`, with or without a `where` in the signature. That is a
separate capture channel (class registration, not sub registration) and is
recorded in `todo/tickets/class-in-routine-does-not-capture-routine-lexicals.md`.

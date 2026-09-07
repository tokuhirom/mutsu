# `[myop] 5` returns 5 where rakudo calls the operator (and dies)

Measured 2026-09-07 while fixing
`todo/tickets/reduce-with-a-bracketed-callable-op-does-not-parse.md`
(`news/2026-09/bracketed-callable-reductions.md`). Independent of it — it
reproduces for every non-builtin reduction operator spelling, not just the
bracketed-Callable one that fix was about.

## Repro

```raku
sub infix:<myop>($a, $b) { $a + $b }
say [myop] 5;        # raku: dies "Too few positionals passed; expected 2 arguments but got 1"
                     # mutsu: 5

sub f($a, $b) { $a + $b }
say ([[&f]] 5);      # raku: same death            mutsu: 5
say ([[&f]]);        # raku: same, "got 0"         mutsu: [f]   (an Array literal!)
```

## What is correct, and why that narrows it

A **builtin** operator has an identity and a documented one-element answer, and
mutsu gets both right:

| Program | raku | mutsu |
|---|---|---|
| `[+] 5` | `5` | `5` — correct |
| `[+]` | `0` | `0` — correct |
| `[*]` | `1` | `1` — correct |

rakudo's reduce metaop only short-circuits the one-element case for operators it
knows an identity for; a user routine has none, so it is simply *called* with
whatever elements there are — and dies on arity. mutsu returns the single
element for every operator, builtin or not.

The zero-element `[[&f]]` case is a second, distinct bug: `reduction_op`'s
zero-argument branch is not reached (the parse falls back to reading `[f]` as an
Array literal), so it prints `[f]` rather than reducing at all.

## Where to look

`Interpreter::exec_reduction_op` (`src/vm/vm_misc_reduction_exec.rs`) — the
`list.len() == 1` early returns in both the scan and fold paths, which answer
`list[0]` before consulting `callable`. They need to call the callable instead
when there is one (letting the binder produce raku's arity error), while keeping
the builtin identity behaviour untouched. And `reduction_op`
(`src/parser/primary/misc/reduction.rs`) for the zero-operand `[[&f]]` spelling,
whose `r.is_empty()` / `r.starts_with(';')` branch is evidently not taken.

## Neighbourhood to check when fixing

`[myop] 5` and `[[&f]] 5` (fold) and their `[\...]` scan twins; a callable whose
arity IS 1 (which must then succeed); `[+] 5` / `[+]` / `[*]` / `[~]` and the
other identity-bearing builtins, which must not move; a set operator's one-arg
coercion (`[(|)] <a>`, which `set_reduction_one_arg` handles deliberately); and
a chain comparison (`[<] 5`, which is `True`).

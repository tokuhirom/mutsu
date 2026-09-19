# A closure could capture the wrong same-named variable when stored via a container element or an expression assign

A closure (`sub { ... }`) that reads a lexical `$path` from its defining
method could resolve to the **wrong** `$path` — one belonging to an
unrelated, same-named parameter in a *different* method somewhere on the
call stack — whenever the closure's own `$path` was assigned anywhere in
its defining method's body, even inside a branch that is provably never
taken at runtime (dead code). Removing the dead assignment made the closure
resolve correctly, which pointed at the closure-capture escape analysis
rather than at dead-code elimination (#8663).

```raku
class Outer {
    method resource(Str $member) {
        my Str $path = $member;
        $path = 'never' if False;          # dead code; never runs
        my %opts;
        %opts<call> = sub ($self) {
            say $path;                     # read the WRONG $path
        };
        Inner.new.invoke(path => '', options => %opts);
    }
}
```

## Root cause

`box_captured_lexicals` only promotes a captured-and-mutated local to a
shared `ContainerRef` cell when the closure that captures it is also marked
as *escaping* its creating frame (stored, returned, or bound — not
immediately invoked). A closure that is neither boxed into a cell nor
vouched for as "never mutated" (`authoritative_free_vars`) falls back to a
plain-value snapshot in its captured env. At call time, the VM's per-call
env chain puts the CALLER's own frame ahead of that snapshot as a fallback
tier — by design, so an escaping closure's shared cell (or its
authoritative snapshot) can win by explicit override, while a plain,
un-vouched snapshot has no such defense. So whichever frame happens to be
running when the closure is finally invoked, a same-named parameter in
*that* frame's own env wins the lookup instead of the closure's own
lexical.

Two compile sites built a closure literal from a syntactic position that
is unambiguously escaping, but never set the compiler's escape-position
flag (`escaping_position`) before compiling it, so the closure was recorded
as non-escaping regardless of where it was actually written:

- `compile_bind_index_value` — the `IndexAssign` RHS compiler, reached by
  `%h<k> = sub {...}`, `@a[i] = sub {...}`, and the deep/nested subscript
  variants. Its sibling, the fix for a closure literal passed as a named
  call argument (`news/2026-08/closure-capture-shadowed-by-colliding-callee-parameter.md`),
  already covered the call-argument shape but not this one.
- `compile_expr_assign` — an assignment used as an *expression*
  (`($cb = sub {...})`, e.g. inside an `if` condition or another
  expression). Its `Stmt::Assign` sibling, `compile_assignment_rhs_for_target`,
  already marked this correctly; the expression-context twin had simply
  never been given the same treatment.

Both now mark a stored closure literal as escaping before compiling it,
exactly like the existing `Stmt::Assign` and named-call-argument sites.

Regression coverage: `t/routines/closure/closure-capture-hash-element-callback.t`
(the reported hash-element shape) and
`t/routines/closure/closure-capture-expr-assign-callback.t` (the
expression-context assign shape found investigating the same root cause).

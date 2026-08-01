# `use fatal` leaks out of a `sub` body and a `do {}` block

Pragmas are lexically scoped. mutsu keeps `use fatal` as an interpreter-wide
flag (`Interpreter::fatal_mode`) and restores it at *some* scope exits but not
all:

```
$ mutsu -e '{ use fatal; my $q = 1 }; my $f = "bar"[5]; say "soft " ~ $f.^name'
soft Failure                       # bare block: correct

$ mutsu -e 'sub s { use fatal; 1 }; s(); my $f = "bar"[5]; say "soft " ~ $f.^name'
Index out of range. Is: 5, ...     # sub body: leaks

$ mutsu -e 'my $x = do { use fatal; 1 }; my $f = "bar"[5]; say "soft " ~ $f.^name'
Index out of range. Is: 5, ...     # do block: leaks
```

`raku` keeps all three scoped.

## Where the working case gets it right

The compiler wraps a *statement* block that contains a `use` in
`OpCode::PushImportScope` / `PopImportScope` (`compiler/stmt.rs`, guarded by
`has_use_stmt`), and `push_import_scope` / `pop_import_scope`
(`runtime/runtime_module.rs`) save and restore `newline_mode`, `strict_mode`,
`fatal_mode` and `monkey_typing` along with the function/class registries. A
`sub` body and an `Expr::DoBlock` do not go through that arm.

Note the arm order in `compiler/stmt.rs`: `has_let_deep` is tested *before*
`has_use_stmt`, so a block containing both `let` and `use` skips the import
scope too — worth checking while fixing this.

## Why it was not fixed with the EVAL leak

`news/2026-08/eval-does-not-leak-use-fatal.md` fixed the EVAL case with a plain
save/restore around `eval_eval_string`, which is what the Test-vendoring sweep
needed (`throws-like 'use fatal; ...'` poisoned every later assertion in the
file). Extending the import scope to routine bodies is a different, wider change
— `pop_import_scope` also un-registers functions and classes declared since the
push, and its doc comment records several hard-won exceptions — so it wants its
own pass and its own roast run rather than riding along.

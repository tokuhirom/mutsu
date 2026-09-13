# A named `my method` / `my submethod` keeps its declarator

`my method foo($x) { 1 }` is a `Method` in raku and `my submethod bar($x) { 1 }` is a `Submethod`.
mutsu registered every one of them — `my` and `our`, method and submethod — as a plain `Sub`.

```
$ raku  -e 'my submethod bar($x) { 1 }; say &bar.WHAT'
(Submethod)
$ mutsu -e 'my submethod bar($x) { 1 }; say &bar.WHAT'
(Sub)
```

The *literal* forms (`method (...) { }`, `submethod (...) { }`, and their `anon` spellings) were
fixed earlier by `8063666da`; this is the named declarator statement, which that change deliberately
left alone.

## Root cause — two independent losses, one on each side of the compiler

[#8313](https://github.com/tokuhirom/mutsu/issues/8313) identified the second of these. The first
turned up only on measuring, because the ticket's diagnosis assumed `Stmt::MethodDecl` already
carried the declarator faithfully:

**The parser never recorded `submethod`.** `my_decl_dispatch.rs`'s `my`/`our` declarator dispatch
had a `submethod` branch that was a copy of the `method` branch: both called the shared
`method_decl_body*` grammar, which leaves `is_submethod` false. So `my submethod bar` and
`my method bar` produced identical ASTs — the keyword was gone before any lowering could drop it.
`--dump-ast` on `my submethod bar($x) { 1 }` showed `is_submethod: false`.

**The lowering dropped it again, and nothing downstream could restore it.** The
`Stmt::MethodDecl` → `Stmt::SubDecl` lowering in `src/compiler/stmt.rs` rewrote the declaration with
`custom_traits` of exactly `[("__mutsu_method_decl", None)]`, so `register_sub` had nothing left to
distinguish the spellings. And the `&name` value for a method declaration is built *in place* in
`register_sub`'s `is_method_value_decl` branch, capturing `self.env.clone()` directly — it never
goes through `sub_value_from_function_def`, which is where the `__mutsu_callable_type` stamp lives.
(A breakpoint on that function proved it: it never fires for `&foo`. Guessing otherwise would have
cost a rebuild per guess.)

## The fix

- The parser's `submethod` branch now sets `is_submethod` on the statement it returns.
- The lowering carries the declarator across as a marker, reusing `RoutineDeclarator::literal_marker`
  — the same markers the closure-building opcode already reads for a `method (...) { }` literal — so
  both routine paths now name the declarator the same way. `RoutineDeclarator::from_markers` and
  `callable_type` were added as the shared readers.
- `FunctionDef`'s `is_method: bool` became `declarator: RoutineDeclarator`. A second boolean beside
  `is_method` would have repeated exactly the smell `8063666da` removed from `Expr::AnonSubParams`,
  and could not have expressed `Submethod` anyway. Thirteen construction sites, three reads.
- `register_sub` stamps the callable type onto the env that the `&name` value captures.

## Pin

`t/oo/method/named-method-declaration-keeps-its-declarator.t` — 12 tests, green under mutsu and
under real Rakudo: all four named spellings, `.^name` and `~~` agreeing with `.WHAT`, and the
neighbours that must not move (`my sub`, `our sub`, and a plain `sub` all still `Sub`).

## A second divergence found, filed rather than fixed

The pin originally also called the routines, and that failed — mutsu and rakudo disagree about the
*signature* a named method value has. rakudo's is `(Mu $:: $x, *%_)`; mutsu's is `($x)`. They are
exact mirrors: `&m(21)` works in mutsu and is an arity error in rakudo, `&m(Any, 21)` and `5.&m(21)`
the other way round. So a `my method` cannot currently be applied to an invocant at all, which is
most of the reason to declare one. That survives this fix and is a wider change — it moves the
registered signature, which feeds `.arity`/`.count`/`.signature` and multi-candidate keying — so it
is filed as [#8348](https://github.com/tokuhirom/mutsu/issues/8348) and the pin says in a comment
why those assertions are absent.

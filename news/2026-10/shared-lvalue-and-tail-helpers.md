# One implementation for variable keys, lvalue spines and a block's last statement

Part of the AST-walker cleanup (#10468). Three small pieces of AST logic had been written out by
hand over and over, each copy slightly different.

**The variable-key atom.** The match `$x` → `x`, `@a` → `@a`, `%h` → `%h` was spelled out at
about 45 places in the parser, compiler and runtime. It is now `Expr::container_var_key` (with
`Expr::var_key`, which adds `&f`, and `Expr::sigiled_var_name` for the spelling a diagnostic
prints, which keeps the reserved `$self` key as it is). The copy in `regex_tree.rs`'s source
renderer used to print `$self` as `$$self`; it now goes through the shared helper and prints
`$self`.

**Lvalue and index-chain spines.** Four functions found the variable an lvalue writes through:
`postfix_index_name`, `index_assign_target_name`, `lvalue_assign_name` and
`chain_root_lvalue_name`. Each looked through a different set of wrappers. They are now one
`Expr::lvalue_root(peel)`, and each caller says which wrappers it looks through. The sets really
do differ. A postfix `++`, `:delete` or mutating method writes through the name without
evaluating the target, so it must not look through `temp (...)`, which saves the variable when
it is evaluated. It must not look through parentheses either: they would expose a
`(my @a = ...)` declaration that would then never run. Element assignment evaluates its target,
so it looks through `temp`. Five more copies of "walk an `Index` chain to its root" became
`Expr::index_root` / `Expr::index_path`. Three more were folded into existing helpers: the two
`"@a\0idx\01"` element-source encoders (parser bind metadata and the compiler's `=:=` check), and
the two `with`/`without` element-source root checks, which must agree with each other.

Unifying the spines fixed a real bug. A sigil-less root was keyed by its spelling, but a
sigil-less `constant` is stored under its term key. So `constant c = [1, 2, 3]; c[0] = 7` and
`c[1]++`, `++c[2]`, `c[0] += 5`, `c[0, 1] = 8, 9` and `c[5] //= 6` were all silently lost. They
now write through, as in rakudo. The method-lvalue writeback name (`$s.substr-rw(0, 1) = "x"`)
was also hand-written at eight parser sites, and the statement forms did not accept a sigil-less
invocant. It is now one `method_lvalue_target_name`. The write-back still worked without it,
because the binding shares the container.

**The last value statement of a block.** Nine call sites each scanned backwards for "the
statement a block evaluates to", with four different skip rules. They now share
`ast::last_value_stmt` with two skip sets. `Markers` skips `SetLine` and the binding markers a
`my \x = ...` lowering appends; both are never values. `MarkersAndPhasers` is used only for the
program's top-level statements, where `run()` moves a `POST` to the end. Checking every caller
against rakudo showed that a trailing phaser *is* the last statement: `do { 42; LEAVE { } }`
and `{ 42; LEAVE { } }()` are `Nil` in rakudo, and a block ending in `KEEP { }; UNDO { }` runs
`UNDO`. mutsu's `do`/bare-block and closure compilers had answered `42` and run `KEEP`. Both now
treat a trailing `LEAVE`/`KEEP`/`UNDO`/`PRE`/`POST` as making the value `Nil`. A trailing
`ENTER` still gives the block its value.

The AST-walker baseline fell by 10 walkers (130 to 120). Four rows left the list entirely
(`expr_data.rs`, `dot_assign.rs`, `let_temp.rs`, `with_desugar.rs`), and `expr_binary.rs`,
`helpers_ast_utils.rs`, `mod.rs`, `stmt.rs` and `run.rs` each lost one or two. Pinned by
`t/collections/constant-sigilless-element-lvalue.t` and `t/control/trailing-phaser-block-value.t`.

# `$`-parameter binding itemizes its argument — CSV::Table suite reaches 10/10

Raku's signature binder places a value bound to a plain `$`-sigiled parameter
into a Scalar container: the bound value is ONE element in list context and
`.raku` shows the leading `$` (`sub f($v) {...}; f([1,2])` sees `$[1, 2]`).
mutsu bound every parameter raw, so a row `Array` handed to `-> $i, $v` by
`@!cell.kv` exploded inside a sprintf slurpy — `sprintf "%-*.*s", $w, $w, $v`
died with "directives specify 3 arguments, but 4 were supplied", which was the
last remaining failure (`t/5-save.t`) in `CSV::Table`'s suite
(`docs/batteries/csv.md`).

The semantics were verified against raku first: `$` params itemize (including
`is copy`, named params, and explicit `$_` params), while `is raw`, `is rw`
(which aliases a container), sigilless (`\v`), `@`/`%`/`&` params, and the
*implicit* topic binding stay raw. `:=` binds never itemize.

Itemization was applied at every parameter-bind site, all funnelling through
one helper (`itemize_plain_scalar_param`) or `itemize_scalar_store` directly:

- **`SetGlobal` scalar stores** (`vm_exec_dispatch.rs`) — the multi-param
  for-loop bind statements (`build_for_bind_stmts` emits plain assignments)
  compile to `SetGlobal` when the name has no local slot, and that path lacked
  the `itemize_scalar_store` call the `SetLocal` path has had all along. This
  also fixes `our $x = [1,2]` and closure-captured scalar assignments reached
  by name. Binds, rebinds, and internal `__*` temporaries are excluded (an
  itemized for-loop source temp would iterate as a single item).
- **for-loop single-param binds** (`vm_for_loop_body.rs`, `vm_for_loop_lazy.rs`
  both variants) — `for @c -> $v` now binds each element itemized. The backing
  `Gc` is shared (only the `ArrayKind` flips), so `loop_var_unchanged`'s
  `ptr_eq` still recognizes in-place mutations and the source-element
  writeback stays a no-op for read-only loops.
- **sub/closure call paths** — `vm_call_light.rs`, `vm_call_light_typed.rs`
  (positional + named), the full binder's positional and named supplied-value
  sites (`binding_signature.rs`), and its legacy params path (pointy blocks
  whose defs don't survive, placeholders `$^a`). rw `ContainerRef` cells pass
  through untouched (no `Array` view), so `is rw` writeback is unaffected.
- **map/grep/first block params** (`resolution_map_grep.rs`) — `-> $v` and
  `$^a` blocks bind itemized; the implicit topic stays raw.

Two consumers assumed bare-bound values and were fixed as general bugs:

- `.cache` on an itemized array returned the still-itemized value; a method
  result is decontainerized in raku, so `for $node.cache` must iterate
  elements. Left itemized, zef's `Zef::Config::plugin-lookup` recursed on the
  same one-element item forever (stack overflow in `mzef --version`).
- `&combinations(Iterable, $k)` treated an itemized first argument as the
  numeric-n form's single element; the spec pins the function form to the
  method form (`roast/S32-list/combinations.t` subtest 33), so it now deconts.

With this, `CSV::Table`'s suite is **10/10 (184 assertions)** under mutsu —
every blocker found by the CSV battery survey on that path is now fixed. The
remaining half of the itemization gap is store-side: array/hash *elements*
read directly (`@d[0].raku`, implicit-topic iteration over `@c`) are still
bare — `todo/deep/element-itemization-lost-in-scalar-binding.md` tracks that
follow-up. Pinned by `t/param-bind-itemization.t` (21 assertions, green under
raku and mutsu).

# EXPORTHOW::DECLARE + HOW-driven class registration — run OO::Monitors verbatim

The `monitor` declarator is currently provided natively (#5640) as an
explicitly TEMPORARY stopgap. The user has ruled that native provision of
ecosystem modules is banned going forward (CLAUDE.md working agreements):
the correct end state is that the real, vendored `OO::Monitors` runs
verbatim, and the native declarator is retired. This ticket is the design
survey for that campaign (deferred to a following session on 2026-08-01).

## What already exists (surveyed 2026-08-01)

- **EXPORTHOW keyword MAPPING for existing declarators works**:
  `EXPORTHOW.WHO.<class> = SomeHOW` from a `use`d module gives classes an
  instance of the user HOW as `.HOW` (`install_custom_class_how`,
  `runtime/metamodel.rs`); a user `compose` runs after the class's traits
  (`registration_class_decl.rs` end, `pending_class_compose`); grammars
  route subrule dispatch through a user `find_method`
  (`install_custom_grammar_how`, GrammarHOW protocol). Proven by the AOP
  `advent2011-day14` example.
- EXPORTHOW directive names are validated (DECLARE / SUPERSEDE / COMPOSE,
  `run_prelude.rs::validate_exporthow_directives`).
- Native ClassHOW metamethods reachable from user HOWs:
  `.^add_method`, `.^compose`, `add_parent`
  (`methods_classhow_dispatch.rs`).
- `Attribute.new(:name, :type, :package)` and
  `.get_value($obj)` / `.set_value($obj, $v)`
  (`methods_object_dispatch_new.rs`, `methods_instance_ops.rs`).
- Routine/method `.wrap` with `callsame` (wrap chains), dynamic vars
  (`$*EXCLUDE-MONITOR-LOCK`, `$*MONITOR`), `anon method`.
- Metamodel primitives: `create_type` / `compose_type` / `rebless`
  (`runtime/metamodel.rs`).

## What is missing (the campaign)

1. **`EXPORTHOW::DECLARE::<keyword>` — a NEW declarator from a module.**
   The parse-time module scan (`stmt/simple/module_exports.rs`) must detect
   `my package EXPORTHOW { package DECLARE { constant monitor = SomeHOW } }`
   and register `keyword → HOW type name` in a parser-side table; a generic
   `declare_decl` statement parser (generalizing the current hardcoded
   `monitor_decl` in `stmt/class/class_decl.rs`) parses `keyword Name {...}`
   like a class and tags the ClassDecl with the declarator's HOW type.
   The unit-scoped enable flag must survive nested module-scan parses
   (the save/restore in `scan_module_source` — same trap as the current
   `MONITOR_DECL_ENABLED` flag).
2. **Drive class registration through the user HOW protocol.** For a
   DECLARE'd type, registration must call the user HOW's overridable hooks
   with `callsame`/`nextsame` bridging to the native implementation:
   - `new_type(|)` → callsame yields the type object; OO::Monitors then
     calls `self.add_attribute(type, $!lock-attr)` (native `^add_attribute`
     bridge must add a real attribute to the registered class).
   - `add_method(type, $name, $meth)` for EVERY method in the body — the
     upstream override `.wrap`s the method (`-> \SELF, | { ... callsame }`)
     and then calls `self.Metamodel::ClassHOW::add_method(...)` (a
     FULLY-QUALIFIED method call on self — parser/dispatch support needed).
     The wrapped chain must be what later method dispatch runs, i.e. the
     registry MethodDef must carry the wrap.
   - `compose(type)` — the override reads `self.method_table(type)`
     (native bridge returning a mutable-enough map), wraps
     BUILDALL/POPULATE (or adds them via add_method), adds a `clone`
     method, then calls `self.Metamodel::ClassHOW::compose(type)`.
   - `attributes(|)` override with `nextsame`/`callsame` (used to hide the
     lock attribute during `.clone` when `$*EXCLUDE-MONITOR-LOCK` is set);
     the `hllize` shim (`$a<>`) should be a no-op under mutsu since
     `^attributes` already returns a Raku list.
3. **Retire the stopgap** in the same campaign: remove the native
   `monitor` gate (`use_decl.rs`, `MONITOR_DECL_ENABLED` hardcoding), the
   `OO::Monitors` native-module arm (`runtime_module.rs`), the
   `__mutsu_monitor` marker + monitor-lock hooks in
   `call_compiled_method(_fast)` (`vm_method_dispatch.rs`) and the monitor
   registry in `native_methods/state_lock.rs` — the real module's wraps
   take over. Vendor the module at `modules/OO-Monitors/` and flip
   `batteries.lock`'s `-` bundled_lib to the vendored lib dir (the `-`
   native marker in `battery-testsuite.sh` can stay for future use or be
   reverted). Keep `t/oo-monitors-battery.t` green throughout — it pins
   behavior, not implementation.

## Estimated shape

Item 1 is moderate (a generalization of machinery that exists twice
already). Item 2 is the deep part — it reifies mutsu's class registration
as an overridable protocol, which is also the enabling step for other
metamodel-based ecosystem modules. 1–3 sessions total. Use the upstream
suite (5 files, 9 subtests, pinned at `5f3af495` in `batteries.lock`) as
the oracle; the 4-threads increment test is the serialization proof.

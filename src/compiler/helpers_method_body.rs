//! Main-pass method-body compilation (ADR-0019 D3-8a).
//!
//! Every class/role method body is compiled once here, at main-pass compile
//! time, mirroring the registration-time throwaway compile
//! (`Interpreter::compile_method_def_in_place_with_dist`,
//! `src/runtime/accessors_resolve.rs`) bit-for-bit — see the design doc
//! (`todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md`) decision 2.
//! D3-8a is purely additive: the resulting [`crate::symbol::Symbol`] key is
//! stashed on [`crate::opcode::CompiledMethodDecl::compiled_routine_key`] but
//! nothing reads it yet — that is the D3-8b/c registration cutover.

use super::*;

impl Compiler {
    /// Compile one class/role method/submethod body to bytecode at main-pass
    /// compile time, keyed into `self.compiled_functions` exactly like a
    /// `sub`'s body (`compile_sub_body`). Returns `None` only if... it never
    /// does today (every static-name declaration compiles); callers guard
    /// the computed-name case themselves by not calling this at all.
    ///
    /// `is_hidden` / `apply_auto_positional_slurpy` reproduce the *exact*
    /// input divergence between the class-body method walker
    /// (`class_body_method_decl`: auto `@_` detection, `is_hidden`-gated
    /// implicit `%_`) and the role-body method walker (`role_body_method_decl`:
    /// no auto `@_` insertion, `is_hidden` always `false`) — callers must
    /// pass the same values their registration counterpart would use, or the
    /// parity guarantee (design decision 2) breaks.
    #[allow(clippy::too_many_arguments)]
    pub(crate) fn compile_method_body(
        &mut self,
        package_name: &str,
        method_name: &str,
        param_defs: &[crate::ast::ParamDef],
        body: &[Stmt],
        is_hidden: bool,
        apply_auto_positional_slurpy: bool,
        is_rw: bool,
        return_type: Option<&String>,
        decl_line: Option<i64>,
    ) -> Option<Symbol> {
        let mut effective_param_defs =
            crate::method_signature_shared::effective_method_param_defs(param_defs, is_hidden);
        // Raku methods never get an implicit `*@_` (unlike subs) -- a
        // signature-less method body that reads a bare `@_` directly is
        // rejected instead, matching the do{}-nested sibling shape
        // (`Compiler::compile_do_block_expr`). Swap in the synthetic
        // die-only body rather than the real one when detected; every other
        // bit of method compilation below (param defs, closure body
        // compilation, fingerprint/key) proceeds unchanged on that body.
        let owned_die_body;
        let body = if crate::method_signature_shared::needs_direct_positional_placeholder_die(
            apply_auto_positional_slurpy && param_defs.is_empty(),
            body,
            &mut effective_param_defs,
        ) {
            owned_die_body =
                crate::method_signature_shared::direct_positional_placeholder_die_body();
            owned_die_body.as_slice()
        } else {
            body
        };
        let effective_params: Vec<String> = effective_param_defs
            .iter()
            .map(|p| p.name.clone())
            .collect();

        // Seeded exactly like `compile_method_def_in_place_with_dist`: a bare
        // `Compiler::new()` (deliberately NOT inheriting this (the main-pass)
        // compiler's enclosing scopes/fold_ctx/outer_code_var_names — design
        // decision 2), the declaring package, the enclosing distribution, and
        // `lexically_in_method` for the implicit `%_`/`@_` lexicals.
        let mut method_compiler = Compiler::new();
        method_compiler.set_current_package(package_name.to_string());
        method_compiler.current_distribution = self.current_distribution.clone();
        method_compiler.lexically_in_method = true;
        // `method m($self: $n)` names its invocant param `self`, so it binds the
        // plain `"self"` key; a `$self` read in the body must resolve to it
        // rather than to the reserved lexical key (ADR-0061). An *anonymous*
        // invocant marker (`method m(Foo:D:)`) declares no `$self` and is
        // excluded by `ParamDef::declares_self_lexical`.
        method_compiler.self_is_signature_param =
            crate::ast::signature_declares_self_lexical(&effective_param_defs);
        let mut method_params: Vec<String> = vec![
            "self".to_string(),
            "__ANON_STATE__".to_string(),
            "?CLASS".to_string(),
            "?ROLE".to_string(),
        ];
        method_params.extend(effective_params.iter().cloned());
        let mut cc = method_compiler.compile_routine_closure_body(
            &method_params,
            &effective_param_defs,
            body,
        );
        cc.compute_may_capture_outer_vars();
        cc.compute_needs_env_sync();
        // Declaration metadata, the same channel `compile_sub_body` and the
        // closure paths use: `Code.line` reads it back off the `MethodDef`'s
        // installed `compiled_code`, so no separate per-method line field is
        // needed anywhere between here and `.^lookup`.
        cc.source_line = decl_line;
        // ADR-0032 D2: bubble this method body's container-capture edges
        // (recorded in `cc.container_ref_capture_syms` by D1, during
        // `method_compiler`'s own independent compile above) to `self` — the
        // enclosing frame at the class/role declaration site — exactly like
        // a nested named sub or closure literal. `method_compiler` is a
        // fresh `Compiler::new()` with no access to `self.local_map`
        // (design decision 2), so this bubbling is the only place a
        // captured outer scalar's owning frame can be told to box it at its
        // declaration. This mutates `self.code` regardless of whether `cc`
        // itself becomes the method body actually invoked at runtime (a
        // registration-time throwaway compile may independently recompile
        // the same source and get its own `container_ref_capture_syms` via
        // the same D1 rule) — Half A only needs to know WHICH names are
        // captured, not which CompiledCode object executes.
        if !cc.container_ref_capture_syms.is_empty() {
            let syms = cc.container_ref_capture_syms.clone();
            self.bubble_container_ref_capture_syms(&syms);
        }

        // Harvest this body's outer-lexical WRITES as a byproduct of the same
        // compile, instead of paying a second, analysis-only
        // `compile_closure_body` per method (what `record_type_body_captures`
        // used to do for every class/role body right before the declaration
        // plan was built). `free_var_writes` & friends are computed by
        // `CompiledCode::compute_free_vars`, which partitions names purely by
        // whether the compiled body owns them as locals — a fact of the body
        // itself, not of the enclosing compiler's scope — so the scope-blind
        // `method_compiler` above yields the same set the outer compiler's
        // analysis compile did. Verified empirically across the whole `t/`
        // suite and the roast whitelist: the two harvests differ only in
        // compiler-minted `__mutsu_*` temporaries' ordinals, which
        // `record_type_body_written_lexicals` filters out either way.
        let type_body_writes: Vec<Symbol> = cc
            .free_var_writes
            .iter()
            .chain(cc.free_var_container_writes.iter())
            .chain(cc.needs_cell_named_sub_free.iter())
            .copied()
            .collect();
        if !type_body_writes.is_empty() {
            self.record_type_body_written_lexicals(type_body_writes);
        }

        // ...and this body's outer-lexical READS, on the same one compile, into
        // the enclosing frame's ordinary closure-capture channel. A method is
        // installed into its type's method table by `RegisterDecl` and, exactly
        // like a nested named `sub` (`compile_sub_body_with_deprecation`'s twin
        // push) and unlike a nested anonymous closure, has no runtime
        // closure-creation op — so it never reaches `closure_compiled_codes`
        // and the enclosing scope's own `compute_free_vars` scan would
        // otherwise never learn that this body references an outer lexical.
        // Without this, `my $l = 42; my &blk = { my class C { method go() { $l
        // } }; C.new.go }; blk()` snapshots a closure env with no `$l` in it
        // and the method reads `Any` (see
        // `news/2026-08/class-method-in-block-free-var-capture.md`).
        // `type_body_written_lexicals` above covers only the WRITES, and does
        // so through a separate, name-keyed runtime lane
        // (`note_type_body_written_lexicals`) that never populates the capture
        // set a block value carries.
        if !cc.free_var_syms.is_empty() {
            self.code
                .nested_routine_free_reads
                .push(cc.free_var_syms.clone());
        }
        // A parameter's DEFAULT VALUE (`method go($x = $l) {...}`) is a second,
        // distinct capture site: it is evaluated from the `ParamDef` AST at
        // call time and never appears in `cc`'s ops at all, so the fold above
        // cannot see it. Same for a `where` constraint. Harvest both the same
        // way the attribute-default side does
        // (`bubble_decl_time_free_reads`).
        let param_default_reads = self.decl_time_param_free_var_syms(&effective_param_defs);
        self.bubble_decl_time_free_reads(param_default_reads);

        // Key shape follows C2 (design decision 5): a `!m` marker keeps
        // method keys disjoint from sub keys, and the fingerprint —
        // computed over the EFFECTIVE params/param_defs/body, matching what
        // registration actually installs — disambiguates same-named multi
        // candidates without needing the sub side's separate
        // signature-vs-fingerprint key scheme (each `CompiledMethodDecl`
        // already owns one key slot; there is no shared dispatch-table
        // lookup key to collide on).
        let arity = effective_param_defs
            .iter()
            .filter(|p| !p.named && (!p.slurpy || p.name == "_capture"))
            .count();
        let fingerprint =
            crate::ast::function_body_fingerprint(&effective_params, &effective_param_defs, body);
        let key_str = format!("{package_name}::{method_name}!m/{arity}#{fingerprint:x}");

        // Merge any nested subs the method body declares into THIS (the
        // real, program-wide) compiler's table, applying the same
        // collision-rename + plan-key remap `compile_sub_body` uses for its
        // own nested declarations.
        let own_compiled_fns =
            self.import_compiled_functions(&mut cc, method_compiler.take_compiled_functions());

        let mut cf = CompiledFunction {
            code: cc,
            source_file: None,
            params: method_params,
            param_defs: effective_param_defs,
            return_type: return_type.cloned(),
            fingerprint,
            // A method always carries the synthetic prefix params, so this
            // is never a genuine empty signature.
            empty_sig: false,
            is_rw,
            is_raw: false,
            is_cached: false,
            param_local_slots: None,
            has_inner_subs: false,
            declares_inner_routines: false,
            named_call_plan: None,
            deprecated_info: None,
            declared_locals: None,
            param_name_syms: Vec::new(),
            package: package_name.to_string(),
            compiled_fns: (!own_compiled_fns.is_empty())
                .then(|| std::sync::Arc::new(own_compiled_fns)),
            memo_cache: std::sync::Arc::new(std::sync::Mutex::new(Vec::new())),
        };
        cf.precompute_param_local_slots();
        cf.precompute_named_call_plan();
        cf.precompute_param_name_syms();
        cf.detect_inner_subs();
        cf.compute_declared_locals();

        let key = Symbol::intern(&key_str);
        self.compiled_functions.insert(key, cf);
        Some(key)
    }

    /// Record `syms` as an extra free-variable contribution to THIS frame's
    /// capture set (`CompiledCode::nested_routine_free_reads`), for a
    /// declaration-time expression that is evaluated from raw AST rather than
    /// from any compiled body — a method parameter's default/`where`
    /// (`method go($x = $l)`) or an attribute's default (`has $.a = $l`).
    /// Both are run at method-call / object-construction time, long after the
    /// declaring block's own frame is gone, and neither contributes ops to a
    /// `CompiledCode` the enclosing free-var scan walks, so without this the
    /// name is missing from the block's closure-env snapshot exactly like a
    /// method body's own reads were (see
    /// `news/2026-08/class-method-in-block-free-var-capture.md`).
    pub(crate) fn bubble_decl_time_free_reads(&mut self, syms: Vec<Symbol>) {
        if !syms.is_empty() {
            self.code.nested_routine_free_reads.push(syms);
        }
    }

    /// Free plain-lexical names referenced by the default values and `where`
    /// constraints of `param_defs`, harvested by compiling each expression as
    /// a standalone analysis chunk (the same throwaway-compile technique
    /// [`Self::record_type_body_captures_uncompiled`] uses). Filtered to plain
    /// user lexicals: a standalone chunk owns no locals, so EVERY name it
    /// touches looks free, and attribute/dynamic/special names resolve through
    /// their own stores rather than the enclosing lexical env.
    pub(crate) fn decl_time_param_free_var_syms(
        &self,
        param_defs: &[crate::ast::ParamDef],
    ) -> Vec<Symbol> {
        let mut out: Vec<Symbol> = Vec::new();
        for pd in param_defs {
            let exprs = pd.default.iter().chain(pd.where_constraint.as_deref());
            for expr in exprs {
                for sym in self.decl_time_expr_free_var_syms(expr) {
                    if !out.contains(&sym) {
                        out.push(sym);
                    }
                }
            }
        }
        out
    }

    /// [`Self::decl_time_param_free_var_syms`] for one expression.
    pub(crate) fn decl_time_expr_free_var_syms(&self, expr: &Expr) -> Vec<Symbol> {
        let mut chunk_compiler = Compiler::new();
        chunk_compiler.set_current_package(self.runtime_current_package().to_string());
        chunk_compiler.current_distribution = self.current_distribution.clone();
        let body = [Stmt::Expr(expr.clone())];
        let (code, _fns) = chunk_compiler.compile(&body);
        code.free_var_syms
            .iter()
            .copied()
            .filter(|sym| sym.with_str(crate::env::is_plain_user_lexical))
            .collect()
    }

    /// The qualified package name a class body's `RegisterClass` op will
    /// resolve at registration time (`exec_register_class_op`,
    /// `src/vm/vm_typedecl_ops.rs`), replicated at compile time for a
    /// statically-named class. `self.current_package` tracks the runtime's
    /// `current_package()` at the point the corresponding `RegisterDecl` op
    /// would execute (both are updated in lockstep by the same
    /// `PackageScope`/`SetCurrentPackage`/unit-package bracketing), so this
    /// produces the same qualified name `class_body_method_decl` sees as
    /// `cx.name` — EXCEPT inside a synthetic STATE-SCOPE pseudo-package
    /// (`current_package` containing `::&`, assigned to every closure/sub
    /// body purely for `state`-variable key uniqueness — see
    /// `compile_sub_body`/`compile_closure_body`), which does not track the
    /// runtime `current_package()` at all. There, `self.enclosing_package`
    /// (captured before the state-scope override, and propagated unchanged
    /// through arbitrarily deep closure nesting) IS the runtime package: a
    /// bare block/closure body never itself changes the interpreter's
    /// current package (only an explicit `class`/`package`/`module`/`unit`
    /// bracketing does, and that always sets `current_package` directly to
    /// the real name, bypassing the mangled form) — see
    /// `qualified_role_decl_name`'s identical rule and ADR-0019 D3-8d.
    pub(super) fn qualified_class_decl_name(
        &self,
        resolved_name: &str,
        is_lexical: bool,
        decl_id: u64,
    ) -> String {
        let base_package: &str = self.runtime_current_package();
        let qualified = if let Some(stripped) = resolved_name.strip_prefix("GLOBAL::") {
            stripped.to_string()
        } else if base_package == "GLOBAL"
            || resolved_name == base_package
            || resolved_name.starts_with(&format!("{base_package}::"))
        {
            resolved_name.to_string()
        } else {
            format!("{base_package}::{resolved_name}")
        };
        // ADR-0047 P1: every `my`/`our`-lexical class or grammar declaration
        // site with a nonzero `decl_id` registers under a MANGLED storage name
        // (`exec_register_class_op`, `Foo\u{0}<decl-id>`), not under its bare
        // qualified name. This function's whole purpose is to predict, at
        // compile time, the exact package name that op will use — so that
        // method bodies and class-body-level `our`/static declarations
        // precompiled HERE (under this predicted name) later resolve the SAME
        // bareword the runtime registration actually binds. Missing this step
        // left every `our $x` inside a `my class` unreachable from its own
        // methods: the declaration was baked "Klass::$x" (this function's
        // pre-ADR-0047 answer) while a method's runtime bareword-fallback
        // lookup used the REAL (mangled) `current_package()` at call time,
        // "Klass\u{0}<id>::$x" — two different strings
        // (`roast/S03-binding/attributes.t`, `roast/S12-attributes/class.t`).
        // This does not replicate the stub+full-definition continuation
        // special case (`lexical_class_pending_stub`) — a stub body has no
        // methods/our-decls of its own to qualify, so that gap is moot here.
        if is_lexical && decl_id != 0 {
            format!("{qualified}\u{0}{decl_id}")
        } else {
            qualified
        }
    }

    /// The role-declaration equivalent of [`Self::qualified_class_decl_name`],
    /// mirroring `exec_register_role_op`'s slightly different (but
    /// equivalent for the unqualified-name case) qualification rule: a role
    /// name that already contains `::` anywhere is left as-is, not just one
    /// that starts with the current package prefix. Uses the same
    /// state-scope `enclosing_package` fallback.
    pub(super) fn qualified_role_decl_name(&self, resolved_name: &str) -> String {
        let base_package: &str = self.runtime_current_package();
        if let Some(stripped) = resolved_name.strip_prefix("GLOBAL::") {
            stripped.to_string()
        } else if resolved_name.contains("::")
            || base_package == "GLOBAL"
            || resolved_name == base_package
        {
            resolved_name.to_string()
        } else {
            format!("{base_package}::{resolved_name}")
        }
    }
}

/// ADR-0019 D3-8a verification item V4: byte-parity between the new
/// main-pass `compile_method_body` compile and the registration-time
/// throwaway compile (`compile_method_def_in_place_with_dist`) it is meant
/// to replace bytecode from later (D3-8b/c). For each sample declaration,
/// the SOURCE-ORDER class/role decl plan's `compiled_routine_key` (looked
/// up post-compile, skipping the `__hoisted` forward-reference shell which
/// this box's own optimization leaves keyless — see `add_class_decl_plan`'s
/// doc comment) must resolve to a `CompiledFunction` whose `code` is
/// `Debug`-identical to the `CompiledCode` the SAME source installs on the
/// registered `MethodDef` at runtime.
#[cfg(test)]
mod d3_8a_byte_parity_tests {
    /// A nested closure/named-sub package is suffixed with a process-global
    /// `STATE_COUNTER` ordinal (`Pkg::&<closure>/N`, `compiler/mod.rs`) so
    /// sibling closures never collide. The two compiles this test pair
    /// performs (a standalone `Compiler::compile` and a full `Interpreter::run`,
    /// which itself compiles far more code — prelude/setting included —
    /// before it reaches the fixture) draw from the SAME global counter at
    /// different starting points, so the ordinal legitimately differs
    /// between the two even when the compiled bytecode is otherwise
    /// identical. Normalize it away before comparing, the same way the
    /// fixtures below only exercise ONE nested-sub case rather than trying
    /// to pin an exact counter value.
    fn normalize_closure_ordinals(s: &str) -> String {
        let marker = "<closure>/";
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while let Some(pos) = rest.find(marker) {
            out.push_str(&rest[..pos + marker.len()]);
            rest = &rest[pos + marker.len()..];
            let digits_end = rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
            out.push('N');
            rest = &rest[digits_end..];
        }
        out.push_str(rest);
        out
    }

    /// `Symbol`'s `Debug` impl prints its raw intern-table index
    /// (`Symbol(91: "foo")`), which is likewise a process-global allocation
    /// order that differs between the two compiles this test pair performs
    /// (see [`normalize_closure_ordinals`]) even when every interned STRING
    /// is identical. Strip the numeric index, keeping only the quoted
    /// content that actually carries meaning.
    fn normalize_symbol_ids(s: &str) -> String {
        let marker = "Symbol(";
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while let Some(pos) = rest.find(marker) {
            out.push_str(&rest[..pos + marker.len()]);
            rest = &rest[pos + marker.len()..];
            let digits_end = rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
            if rest[digits_end..].starts_with(": ") {
                rest = &rest[digits_end + 2..];
            } else {
                // Not the `Symbol(NNN: "...")` Debug shape after all — leave
                // the digits (if any) untouched.
                out.push_str(&rest[..digits_end]);
                rest = &rest[digits_end..];
            }
        }
        out.push_str(rest);
        out
    }

    /// An instance value baked directly into bytecode as a `LoadConst`
    /// (e.g. the `X::Placeholder::Block` this box's own placeholder-die
    /// bodies embed) carries a process-wide auto-incrementing identity `id`
    /// field in its `Debug` output — assigned at construction time, so it
    /// differs between the two independent `Value::make_instance` calls this
    /// test pair's main-pass and runtime-registration compiles each make,
    /// exactly like [`normalize_symbol_ids`]/[`normalize_closure_ordinals`]
    /// already normalize away for the same reason. Only matches a genuine
    /// `id: NNN` field (word-boundary-guarded so it cannot clip the tail of
    /// an unrelated field name).
    fn normalize_instance_ids(s: &str) -> String {
        let marker = "id: ";
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while let Some(pos) = rest.find(marker) {
            let boundary_ok = rest[..pos]
                .chars()
                .next_back()
                .is_none_or(|c| !c.is_alphanumeric() && c != '_');
            out.push_str(&rest[..pos + marker.len()]);
            rest = &rest[pos + marker.len()..];
            if !boundary_ok {
                continue;
            }
            let digits_end = rest
                .find(|c: char| !c.is_ascii_digit())
                .unwrap_or(rest.len());
            if digits_end > 0 {
                out.push('N');
                rest = &rest[digits_end..];
            }
        }
        out.push_str(rest);
        out
    }

    /// An instance's attribute map (`InstanceAttrs`) is backed by an
    /// `FxHashMap`, whose iteration/`Debug` order is a function of each
    /// key's `Symbol` hash — which, per [`normalize_symbol_ids`]'s doc
    /// comment, differs between the two independent compiles this test
    /// pair performs even for an identically-ordered sequence of
    /// `attrs.insert(...)` calls (each compile interns symbols against a
    /// different global table state, landing the *same* string in a
    /// *different* bucket). Normalizing the printed `Symbol(...)` text
    /// alone (`normalize_symbol_ids`) does not fix this: the order was
    /// already baked in before formatting. Find each `AttrMap({...})`
    /// block and sort its top-level `Key: Value` entries lexically so the
    /// comparison is order-independent, mirroring what a real `AttrMap`
    /// (semantically a set of key/value pairs, not an ordered list) should
    /// be compared as.
    fn normalize_attr_map_order(s: &str) -> String {
        let marker = "AttrMap({";
        let mut out = String::with_capacity(s.len());
        let mut rest = s;
        while let Some(pos) = rest.find(marker) {
            out.push_str(&rest[..pos + marker.len()]);
            rest = &rest[pos + marker.len()..];
            // Scan to the matching `})`, tracking nesting depth over every
            // bracket kind that can appear in a Debug-formatted `Value`.
            let mut depth = 0i32;
            let mut end = None;
            for (i, c) in rest.char_indices() {
                match c {
                    '{' | '(' | '[' => depth += 1,
                    '}' | ')' | ']' => {
                        if depth == 0 && c == '}' && rest[i + 1..].starts_with(')') {
                            end = Some(i);
                            break;
                        }
                        depth -= 1;
                    }
                    _ => {}
                }
            }
            let Some(end) = end else {
                // No matching close found (shouldn't happen for well-formed
                // Debug output) -- leave the rest untouched rather than panic.
                out.push_str(rest);
                return out;
            };
            let body = &rest[..end];
            let mut entries: Vec<&str> = Vec::new();
            let mut entry_start = 0usize;
            let mut depth = 0i32;
            let bytes = body.as_bytes();
            let mut i = 0usize;
            while i < body.len() {
                match bytes[i] {
                    b'{' | b'(' | b'[' => depth += 1,
                    b'}' | b')' | b']' => depth -= 1,
                    b',' if depth == 0 => {
                        entries.push(body[entry_start..i].trim());
                        entry_start = i + 1;
                    }
                    _ => {}
                }
                i += 1;
            }
            let last = body[entry_start..].trim();
            if !last.is_empty() {
                entries.push(last);
            }
            entries.sort_unstable();
            out.push_str(&entries.join(", "));
            rest = &rest[end..];
        }
        out.push_str(rest);
        out
    }

    /// Compile `source` two ways and return `(Debug of the main-pass
    /// compiled method body's CompiledCode, Debug of the runtime-registered
    /// MethodDef's CompiledCode)` for `method_name` on `type_name` (a class
    /// or role name). Panics with a descriptive message if either side
    /// cannot find the declaration — a test-fixture bug, not a parity
    /// failure, so it must not silently report "equal".
    fn compiled_code_pair(source: &str, type_name: &str, method_name: &str) -> (String, String) {
        // Main-pass side: parse + compile, then find the SOURCE-ORDER class
        // or role decl plan (the `__hoisted` shell's method_decls are all
        // `None` by this box's own optimization, so the first plan with a
        // `Some` key for this method is unambiguously the real one).
        let (stmts, _) =
            crate::parse_dispatch::parse_source(source).expect("fixture source parses");
        let (code, compiled_fns) = super::Compiler::new().compile(&stmts);
        let key = code
            .class_decl_plans
            .iter()
            .filter(|p| p.name.as_str() == type_name)
            .find_map(|p| {
                p.method_decls
                    .iter()
                    .find(|m| m.name.as_str() == method_name && m.compiled_routine_key.is_some())
                    .and_then(|m| m.compiled_routine_key)
            })
            .or_else(|| {
                code.role_decl_plans
                    .iter()
                    .filter(|p| p.name.as_str() == type_name)
                    .find_map(|p| {
                        p.method_decls
                            .iter()
                            .find(|m| {
                                m.name.as_str() == method_name && m.compiled_routine_key.is_some()
                            })
                            .and_then(|m| m.compiled_routine_key)
                    })
            })
            .unwrap_or_else(|| {
                panic!(
                    "no compiled_routine_key found for {type_name}::{method_name} \
                     (declaration plan missing or method name mismatched)"
                )
            });
        let compiled_fn = compiled_fns
            .get(&key)
            .unwrap_or_else(|| panic!("compiled_routine_key for {type_name}::{method_name} did not resolve in compiled_fns"));
        let main_pass_debug = format!("{:?}", compiled_fn.code);

        // Runtime side: actually run the same source (declares the type,
        // which eagerly compiles every method body — `compile_class_methods`
        // / `compile_role_methods` run right after `RegisterClass/Role`) and
        // read the installed `MethodDef::compiled_code` back out.
        let mut interp = crate::runtime::Interpreter::new();
        interp
            .run(source)
            .unwrap_or_else(|e| panic!("fixture source runs: {e:?}"));
        let registry = interp.registry();
        let overloads = registry.get_method_overloads_with_role_fallback(type_name, method_name);
        let runtime_code = overloads
            .as_ref()
            .and_then(|defs| defs.first())
            .and_then(|def| def.compiled_code.as_ref())
            .unwrap_or_else(|| {
                panic!("no registered compiled_code for {type_name}::{method_name}")
            });
        let runtime_debug = format!("{runtime_code:?}");
        let normalize = |s: &str| {
            normalize_attr_map_order(&normalize_instance_ids(&normalize_symbol_ids(
                &normalize_closure_ordinals(s),
            )))
        };
        (normalize(&main_pass_debug), normalize(&runtime_debug))
    }

    #[test]
    fn plain_method_byte_parity() {
        let source = "class Animal { method speak { 'generic sound' } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Animal", "speak");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn submethod_with_attribute_bind_byte_parity() {
        let source = "class Point { has $.x; has $.y; submethod BUILD(:$!x, :$!y) { } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Point", "BUILD");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn typed_param_method_byte_parity() {
        let source = "class Greeter { method greet(Str $name) { \"hi $name\" } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Greeter", "greet");
        assert_eq!(main_pass, runtime);
    }

    /// Verification item V1: registration substitutes `::?CLASS` in a
    /// param's `type_constraint` string with the resolved class name before
    /// building the installed `MethodDef` (`class_body_method_decl`), so the
    /// runtime side's `compile_method_def_in_place_with_dist` compiles from
    /// `param_defs` with the constraint already rewritten to `"Box"`. This
    /// box's `compile_method_body` deliberately does NOT perform that
    /// substitution (design decision 3) — it compiles straight from the raw
    /// `"::?CLASS"` constraint. Byte-parity holding here is the empirical
    /// confirmation that a type-constraint STRING is bind-time-only data
    /// `compile_routine_closure_body` never bakes into opcodes.
    #[test]
    fn class_pseudo_type_constraint_substitution_does_not_affect_bytecode() {
        let source = "class Box { method store(::?CLASS $other) { 1 } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Box", "store");
        assert_eq!(main_pass, runtime);
    }

    /// Verification item V3: `$?DISTRIBUTION` bakes `Compiler::current_distribution`
    /// directly into the compiled body as a `LoadConst` (`expr_helpers.rs`),
    /// so it is the most direct probe available for whether
    /// `Interpreter::resolve_package_distribution` (the runtime side's
    /// derivation, `compile_class_methods`/`compile_role_methods`) and the
    /// main-pass compiler's `current_distribution` field ever disagree for a
    /// class/role declared in its own compilation unit — the only case
    /// D3-8a computes a key for. A plain in-memory fixture carries no
    /// META6.json, so both sides resolve to `Nil` here either way; this
    /// pins the baseline (no divergence when there is nothing to diverge
    /// over) while the fuller claim — that the two derivations track the
    /// same value even when a real distribution IS loaded — rests on
    /// reading `resolve_package_distribution`'s package-prefix walk and
    /// confirming it always terminates at the same "this compilation unit's
    /// own distribution" value the compiler was seeded with (see the D3-8a
    /// PR description's V3 writeup) rather than on a filesystem-backed test.
    #[test]
    fn distribution_pseudo_var_byte_parity() {
        let source = "class Meta { method dist { $?DISTRIBUTION } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Meta", "dist");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn multi_method_byte_parity() {
        let source = "class Eater {
            multi method eat(Int $n) { \"eating $n\" }
            multi method eat(Str $s) { \"eating $s\" }
        }";
        let (main_pass, runtime) = compiled_code_pair(source, "Eater", "eat");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn is_hidden_class_method_byte_parity() {
        // `is hidden` suppresses the implicit `*%_` (effective_method_param_defs's
        // `class_is_hidden` gate) — cover that branch explicitly.
        let source = "class Hush is hidden { method quiet { 1 } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Hush", "quiet");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn auto_positional_slurpy_method_byte_parity() {
        // No explicit signature but reads bare `@_` directly -> Raku methods
        // never get an implicit `*@_` (unlike subs), so the class-body
        // walker swaps in the synthetic die-only body
        // (`needs_direct_positional_placeholder_die`) on both the main-pass
        // and runtime-registration sides.
        let source = "class Sink { method drain { @_.elems } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Sink", "drain");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn method_with_nested_sub_byte_parity() {
        let source = "class Nested { method outer { my sub helper { 1 }; helper() } }";
        let (main_pass, runtime) = compiled_code_pair(source, "Nested", "outer");
        assert_eq!(main_pass, runtime);
    }

    #[test]
    fn role_method_byte_parity() {
        // Role methods never get the class walker's auto-`@_` insertion and
        // always pass `is_hidden: false` — cover the role-side branch.
        let source = "role Greeter2 { method hello { 'hello' } }
                       class UsesGreeter does Greeter2 { }";
        let (main_pass, runtime) = compiled_code_pair(source, "Greeter2", "hello");
        assert_eq!(main_pass, runtime);
    }

    /// The outer-frame lexicals a compilation unit's class/role method bodies
    /// write, as recorded in `CompiledCode::type_body_written_lexicals`.
    fn type_body_written_lexicals(source: &str) -> Vec<String> {
        let (stmts, _) =
            crate::parse_dispatch::parse_source(source).expect("fixture source parses");
        let (code, _) = super::Compiler::new().compile(&stmts);
        let mut names: Vec<String> = code
            .type_body_written_lexicals
            .iter()
            .map(|s| {
                s.resolve()
                    .trim_start_matches(['$', '@', '%', '&'])
                    .to_string()
            })
            .collect();
        names.sort();
        names.dedup();
        names
    }

    /// The capture harvest is a byproduct of `compile_method_body`'s single
    /// compile now (it used to be a second, analysis-only `compile_closure_body`
    /// per method in `record_type_body_captures`). A frame lexical a method
    /// writes must still reach `type_body_written_lexicals`, or
    /// `clone_for_thread_for_block` drops it off the name-keyed shared lane
    /// (pins: t/destroy-cross-thread-writeback-coherence.t,
    /// roast/S12-construction/roles-6e.t).
    ///
    /// `my $a = 0;` counts as a runtime statement, so `class Foo` here is ALSO
    /// hoisted into a `__hoisted` forward-reference shell — whose own plan
    /// deliberately records nothing. This therefore pins both halves: the shell
    /// staying silent must not cost the real declaration its harvest.
    #[test]
    fn class_method_outer_lexical_write_is_recorded() {
        let names = type_body_written_lexicals("my $a = 0; class Foo { method bump { $a++ } }");
        assert!(names.contains(&"a".to_string()), "got {names:?}");
    }

    #[test]
    fn role_method_outer_lexical_write_is_recorded() {
        let names = type_body_written_lexicals("my $a = 0; role R { method bump { $a++ } }");
        assert!(names.contains(&"a".to_string()), "got {names:?}");
    }

    /// A class declared before any runtime statement is NOT hoisted, so only
    /// one declaration plan exists — the harvest must come from it alone.
    #[test]
    fn unhoisted_class_method_outer_lexical_write_is_recorded() {
        let names = type_body_written_lexicals("class Foo { method bump { $a++ } }; my $a = 0;");
        assert!(names.contains(&"a".to_string()), "got {names:?}");
    }

    /// A computed class name leaves `package_name` `None`, so no main-pass
    /// method-body compile happens and the analysis-only fallback
    /// (`record_type_body_captures_uncompiled`) is the only thing that can
    /// record the write.
    #[test]
    fn computed_name_class_method_outer_lexical_write_is_recorded() {
        let names = type_body_written_lexicals(
            "my $a = 0; my $n = 'Foo'; class ::($n) { method bump { $a++ } }",
        );
        assert!(names.contains(&"a".to_string()), "got {names:?}");
    }

    /// Compiler-minted temporaries (`__mutsu_call_result_7`, ...) are not frame
    /// lexicals and their ordinals are not stable across independent compiles,
    /// so the filter must keep them out.
    #[test]
    fn compiler_temporaries_are_not_recorded() {
        let names = type_body_written_lexicals(
            "my @a = 1, 2; class Foo { method bump { @a[0] = f(@a[1]) } }; sub f($x) { $x }",
        );
        assert!(!names.iter().any(|n| n.starts_with('_')), "got {names:?}");
    }

    #[test]
    fn role_method_auto_positional_slurpy_not_applied() {
        // A role method body reading bare `@_` must NOT get an auto-inserted
        // `*@_` (unlike the class-body walker) — mirrors
        // `role_body_method_decl` never calling `auto_signature_uses`.
        let source = "role Passthru { method relay { @_.elems } }
                       class UsesPassthru does Passthru { }";
        let (main_pass, runtime) = compiled_code_pair(source, "Passthru", "relay");
        assert_eq!(main_pass, runtime);
    }
}

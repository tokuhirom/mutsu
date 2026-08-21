use super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

/// Which enclosing lexical scope an `OUTER::` / `OUTERS::` access names
/// (packages.rakudoc: "OUTER  Symbols in the next outer lexical scope" /
/// "OUTERS  Symbols in any outer lexical scope").
pub(crate) enum OuterStash {
    /// `OUTER::` (chained: `OUTER::OUTER::` is depth 2) -- exactly `depth` scopes out.
    At(usize),
    /// `OUTERS::` -- the innermost enclosing scope that declares the name.
    Any,
}

/// Snapshot of the compiler's lexical-scope-sensitive state, saved on block
/// entry and restored on block exit.
pub(super) struct LexicalScopeSnapshot {
    dynamic_scope_all: bool,
    dynamic_scope_names: Option<std::collections::HashSet<String>>,
    user_listop_shadows: std::collections::HashSet<String>,
    constant_vars_in_scope: std::collections::HashSet<String>,
    constant_vars_current_scope: std::collections::HashSet<String>,
    constant_values: std::collections::HashMap<String, Value>,
    my_vars_current_scope: std::collections::HashSet<String>,
    class_names_current_scope: std::collections::HashSet<String>,
    accessed_dynamic_vars: std::collections::HashSet<String>,
    /// True when this push left `accessed_dynamic_vars` untouched (a
    /// transparent synthetic-wrapper inlining, not a real scope) — see
    /// `push_dynamic_scope_lexical`. The matching pop must then also leave it
    /// untouched, discarding any reads recorded WHILE this frame was open
    /// would wrongly un-track them from the enclosing real scope.
    accessed_dynamic_vars_transparent: bool,
}

impl Compiler {
    fn normalize_dynamic_scope_name(name: &str) -> String {
        name.trim_start_matches(['$', '@', '%', '&']).to_string()
    }

    pub(super) fn push_dynamic_scope_lexical(&mut self) -> LexicalScopeSnapshot {
        // Enter a fresh local-slot scope frame (§1.4 groundwork; inert today —
        // `declare_local` still shares the outer slot for a nested `my $x`).
        self.push_local_scope();
        // Consumed one-shot: when set, THIS push must not reset
        // `accessed_dynamic_vars` — see the field's doc comment. Used only by
        // the recursive `compile_block_inline` call that inlines a
        // `Stmt::SyntheticBlock`'s body in tail position, which is not a real
        // lexical scope.
        let dynamic_reads_transparent =
            std::mem::take(&mut self.next_dynamic_scope_inline_transparent);
        // `std::mem::take` resets the current-scope constant set: the entered
        // block starts with no constants of its own, so an inner `constant X`
        // may legitimately shadow an outer one without being a redeclaration.
        LexicalScopeSnapshot {
            dynamic_scope_all: self.dynamic_scope_all,
            dynamic_scope_names: self.dynamic_scope_names.clone(),
            user_listop_shadows: self.user_listop_shadows.clone(),
            constant_vars_in_scope: self.constant_vars_in_scope.clone(),
            constant_vars_current_scope: std::mem::take(&mut self.constant_vars_current_scope),
            // Inlinable constant values follow the same lifecycle: one declared
            // inside the entered block stops being inlined once it exits (it is
            // then reached as an `our`-scoped package symbol), and an inner
            // constant may shadow an outer one for the block's duration.
            constant_values: self.constant_values.clone(),
            // The entered block starts with no `my` vars of its own, so a same-named
            // `my` inside it shadows (rather than redeclares) an outer one.
            my_vars_current_scope: std::mem::take(&mut self.my_vars_current_scope),
            // Likewise a same-named class inside an inner block shadows rather
            // than redeclares the outer one.
            class_names_current_scope: std::mem::take(&mut self.class_names_current_scope),
            // The entered block starts with no dynamic-var reads recorded of
            // its own: X::Dynamic::Postdeclaration must only fire for a `my
            // $*x` that follows an earlier read of `$*x` in the SAME block —
            // an outer or sibling scope's read is irrelevant (and a read
            // inside a NESTED block that this block encloses is discarded
            // when that nested block's scope pops, so it never leaks back
            // out to a later declaration here either).
            //
            // When `dynamic_reads_transparent` is set, this push is inlining a
            // synthetic wrapper rather than entering a real scope, so leave
            // `self.accessed_dynamic_vars` untouched entirely (not even
            // cloned into the snapshot — the matching pop skips restoring it,
            // see `accessed_dynamic_vars_transparent`).
            accessed_dynamic_vars: if dynamic_reads_transparent {
                std::collections::HashSet::new()
            } else {
                std::mem::take(&mut self.accessed_dynamic_vars)
            },
            accessed_dynamic_vars_transparent: dynamic_reads_transparent,
        }
    }

    pub(super) fn pop_dynamic_scope_lexical(&mut self, saved: LexicalScopeSnapshot) {
        // Drop the exiting block's local-slot scope frame (§1.4 groundwork).
        self.pop_local_scope();
        self.dynamic_scope_all = saved.dynamic_scope_all;
        self.dynamic_scope_names = saved.dynamic_scope_names;
        self.user_listop_shadows = saved.user_listop_shadows;
        // Constants declared inside the exiting block are `our`-scoped: they stay
        // installed in the package, but their lexical local slot is no longer
        // valid, so drop them from the in-scope set. Subsequent bare-word access
        // then resolves them via GetBareWord (package/global lookup).
        self.constant_vars_in_scope = saved.constant_vars_in_scope;
        self.constant_vars_current_scope = saved.constant_vars_current_scope;
        self.constant_values = saved.constant_values;
        self.my_vars_current_scope = saved.my_vars_current_scope;
        self.class_names_current_scope = saved.class_names_current_scope;
        // A transparent push (see `push_dynamic_scope_lexical`) never touched
        // `accessed_dynamic_vars`, so the matching pop must not touch it
        // either — restoring the (empty, unused) snapshot value here would
        // wrongly erase any reads recorded while this frame was "open".
        if !saved.accessed_dynamic_vars_transparent {
            self.accessed_dynamic_vars = saved.accessed_dynamic_vars;
        }
    }

    pub(super) fn apply_dynamic_scope_pragma(&mut self, arg: Option<&Expr>) {
        match arg {
            None => {
                self.dynamic_scope_all = true;
                self.dynamic_scope_names = None;
            }
            Some(Expr::ArrayLiteral(items)) => {
                let mut names = std::collections::HashSet::new();
                for item in items {
                    if let Expr::Literal(lit) = item
                        && let ValueView::Str(s) = lit.view()
                    {
                        names.insert(Self::normalize_dynamic_scope_name(&s));
                    }
                }
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(Expr::Literal(lit)) if matches!(lit.view(), ValueView::Str(_)) => {
                let mut names = std::collections::HashSet::new();
                if let ValueView::Str(s) = lit.view() {
                    names.insert(Self::normalize_dynamic_scope_name(&s));
                }
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(names);
            }
            Some(_) => {
                self.dynamic_scope_all = false;
                self.dynamic_scope_names = Some(std::collections::HashSet::new());
            }
        }
    }

    /// Check if a variable name is a dynamic variable with a package-like name (contains ::).
    pub(super) fn is_dynamic_package_var(name: &str) -> bool {
        let stripped = name.trim_start_matches(['$', '@', '%', '&']);
        if let Some(after_star) = stripped.strip_prefix('*') {
            after_star.contains("::")
        } else {
            false
        }
    }

    /// Whether `name` (an internal `*`-prefixed dynamic var name, e.g. `*TOLERANCE`)
    /// is a built-in dynamic variable provided by the runtime. Built-in dynamics are
    /// always "declared" by the setting, so reading one and *then* shadowing it with
    /// a `my $*X` is legal and must NOT trip X::Dynamic::Postdeclaration (which only
    /// applies to a genuinely user-declared dynamic used before its declaration).
    pub(super) fn is_builtin_dynamic_var(name: &str) -> bool {
        let bare = name.trim_start_matches(['$', '@', '%', '&']);
        let Some(bare) = bare.strip_prefix('*') else {
            return false;
        };
        matches!(
            bare,
            "OUT"
                | "ERR"
                | "IN"
                | "ARGFILES"
                | "ARGS"
                | "SPEC"
                | "CWD"
                | "TMPDIR"
                | "HOME"
                | "EXECUTABLE"
                | "EXECUTABLE-NAME"
                | "PROGRAM"
                | "PROGRAM-NAME"
                | "DISTRO"
                | "PERL"
                | "RAKU"
                | "VM"
                | "KERNEL"
                | "PID"
                | "TOLERANCE"
                | "COLLATION"
                | "DEFAULT-READ-ELEMS"
                | "INIT-INSTANT"
                | "REPO"
                | "RAT-OVERFLOW"
                | "SCHEDULER"
                | "THREAD"
                | "SAMPLER"
                | "USER"
                | "GROUP"
                | "LANG"
        )
    }

    /// Emit X::Dynamic::Package error for a dynamic variable with :: in name.
    pub(super) fn emit_dynamic_package_error(&mut self, name: &str) {
        let symbol = Self::dynamic_var_symbol(name);
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("symbol".to_string(), Value::str(symbol));
        let err = Value::make_instance(Symbol::intern("X::Dynamic::Package"), attrs);
        let idx = self.code.add_constant(err);
        self.code.emit(OpCode::LoadConst(idx));
        self.code.emit(OpCode::Die);
    }

    /// Check a dynamic-variable declaration (`my $*x` / `my $*x := ...`) for the
    /// two compile-time errors that apply to it -- X::Dynamic::Package (a
    /// package-like name) and X::Dynamic::Postdeclaration (the SAME name was
    /// already read earlier in this exact lexical block, per
    /// `accessed_dynamic_vars`'s scoped lifecycle -- see
    /// `push_dynamic_scope_lexical`). Emits the error and returns `true` when
    /// one applies, so the caller can stop compiling this declaration; returns
    /// `false` (emitting nothing) otherwise.
    ///
    /// Shared between the ordinary `Stmt::VarDecl` compile arm and the
    /// block-final (tail-position) `VarDecl` arm in `compile_block_inline` --
    /// the latter is a separate, hand-inlined compile path (needed so a
    /// block-final declaration yields its value) that must not silently skip
    /// these checks just because the declaration is the block's last
    /// statement.
    pub(super) fn check_dynamic_var_decl_errors(&mut self, name: &str) -> bool {
        if Self::is_dynamic_package_var(name) {
            self.emit_dynamic_package_error(name);
            return true;
        }
        if name.starts_with('*')
            && self.accessed_dynamic_vars.contains(name)
            && !Self::is_builtin_dynamic_var(name)
        {
            let symbol = Self::dynamic_var_symbol(name);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("symbol".to_string(), Value::str(symbol));
            let err = Value::make_instance(Symbol::intern("X::Dynamic::Postdeclaration"), attrs);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            return true;
        }
        false
    }

    /// If `name` is a `sub`/`multi sub` declaration of a reserved special-form
    /// operator (one handled directly by the compiler grammar and not
    /// user-overridable), return an X::Syntax::Extension::SpecialForm error value
    /// carrying `category` and `opname`. Returns None for any normal operator
    /// (e.g. `infix:<+>`) or non-operator sub name.
    ///
    /// The reserved set matches Rakudo: `infix:<=>` (assignment), `infix:<:=>`
    /// and `infix:<::=>` (bind), `infix:<~~>` (smartmatch), `prefix:<|>` (flatten).
    pub(super) fn check_special_form_override(name: &str) -> Option<Value> {
        // Split `<category>:<...op...>` into the category and the delimited op.
        let (category, rest) = name.split_once(':')?;
        if !matches!(
            category,
            "prefix" | "infix" | "postfix" | "circumfix" | "postcircumfix"
        ) {
            return None;
        }
        // Strip the angle/French-quote delimiters around the operator name.
        let opname = rest
            .strip_prefix('<')
            .and_then(|s| s.strip_suffix('>'))
            .or_else(|| {
                rest.strip_prefix('\u{ab}')
                    .and_then(|s| s.strip_suffix('\u{bb}'))
            })?
            .trim();
        let reserved = match category {
            "infix" => matches!(opname, "=" | ":=" | "::=" | "~~"),
            "prefix" => opname == "|",
            _ => false,
        };
        if !reserved {
            return None;
        }
        let msg = format!(
            "Cannot override {} operator '{}', as it is a special form handled directly by the compiler",
            category, opname
        );
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("category".to_string(), Value::str(category.to_string()));
        attrs.insert("opname".to_string(), Value::str(opname.to_string()));
        attrs.insert("message".to_string(), Value::str(msg));
        Some(Value::make_instance(
            Symbol::intern("X::Syntax::Extension::SpecialForm"),
            attrs,
        ))
    }

    /// Reconstruct the full symbol name (with sigil) from the internal name.
    pub(super) fn dynamic_var_symbol(name: &str) -> String {
        // If name starts with a sigil (@, %, &), it already has the sigil
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            name.to_string()
        } else if name.starts_with('*') {
            // $* variable — sigil $ was stripped
            format!("${}", name)
        } else {
            format!("${}", name)
        }
    }

    pub(super) fn var_is_dynamic(&self, name: &str) -> bool {
        if self.dynamic_scope_all {
            return true;
        }
        let Some(names) = &self.dynamic_scope_names else {
            return false;
        };
        names.contains(&Self::normalize_dynamic_scope_name(name))
    }

    /// Parse CALLER:: prefix(es) from a variable name.
    /// Returns (bare_name, depth) where depth is the number of CALLER:: levels.
    /// E.g. "CALLER::a" -> ("a", 1), "CALLER::CALLER::a" -> ("a", 2).
    pub(crate) fn parse_caller_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("CALLER::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }

    /// Parse CALLERS:: prefix(es), the twin of [`parse_caller_prefix`].
    /// "CALLERS::a" -> ("a", 1), "CALLERS::CALLERS::a" -> ("a", 2).
    ///
    /// `CALLERS::` differs from `CALLER::` only for a `$*`-twigil dynamic name,
    /// which it cascades outward through the whole caller chain; a plain (non-
    /// twigil) name resolves to the exact frame at `depth`, identically to
    /// `CALLER::` (raku, 2026-07-17: a non-twigil `is dynamic` lexical two callers
    /// out is NOT found by `$CALLERS::`). The `depth`/`cascade` split is decided by
    /// the caller from the returned bare name's twigil.
    pub(crate) fn parse_callers_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("CALLERS::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }

    /// Whether a `CALLERS::` bare name cascades: true for a `$*`-twigil dynamic
    /// name (the sigil already stripped, so it begins with `*`), false otherwise.
    pub(crate) fn callers_name_cascades(bare: &str) -> bool {
        bare.starts_with('*')
    }

    /// Which lexical-scope walk an `OUTER::` / `OUTERS::` pseudo-stash names.
    /// Returns `None` for any other stash (`MY::`, `CORE::`, a real package, ...).
    pub(crate) fn parse_outer_stash(stash: &str) -> Option<OuterStash> {
        if let Some((rest, depth)) = Self::parse_outer_prefix(stash)
            && rest.is_empty()
        {
            return Some(OuterStash::At(depth));
        }
        if Self::parse_outers_prefix(stash).is_some_and(|rest| rest.is_empty()) {
            return Some(OuterStash::Any);
        }
        None
    }

    /// Parse a single `OUTERS::` prefix from a variable name, returning the bare
    /// name. Unlike `OUTER::`, `OUTERS::` does not chain: it already means "any
    /// outer scope", so raku reports `$OUTERS::OUTERS::y` as Nil. Stripping only
    /// the one prefix reproduces that -- the leftover `OUTERS::y` is not a
    /// declared name anywhere, so the lookup misses.
    pub(crate) fn parse_outers_prefix(name: &str) -> Option<String> {
        name.strip_prefix("OUTERS::").map(str::to_string)
    }

    /// Parse `OUTER::` / `OUTER::OUTER::` prefix from a variable name.
    /// Returns (bare_name, depth) where depth is the number of OUTER:: prefixes.
    pub(crate) fn parse_outer_prefix(name: &str) -> Option<(String, usize)> {
        let mut remaining = name;
        let mut depth = 0;
        while let Some(rest) = remaining.strip_prefix("OUTER::") {
            depth += 1;
            remaining = rest;
        }
        if depth > 0 {
            Some((remaining.to_string(), depth))
        } else {
            None
        }
    }
}

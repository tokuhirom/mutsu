use super::*;

/// Bare-callable routines that the VM dispatches directly but that are *not*
/// listed in `BUILTIN_FUNCTION_NAMES` (the latter feeds VM dispatch decisions,
/// so it is kept deliberately narrow). Used only by the EVAL undeclared-routine
/// pre-pass to avoid false-flagging a legitimate builtin / statement keyword as
/// an undeclared user routine. Mirrors the bare-function `match name` dispatch
/// in `builtins.rs` plus the module/import statement keywords that parse as
/// calls (`require`, `use`, `need`, `import`).
const EVAL_KNOWN_ROUTINE_NAMES: &[&str] = &[
    "atomic-fetch",
    "await",
    "bag",
    "cache",
    "caller",
    "callframe",
    "callsame",
    "callwith",
    "categorize",
    "chroot",
    "classify",
    "cr",
    "crlf",
    "cross",
    "dd",
    "deepmap",
    "duckmap",
    "emit",
    "fail",
    "first",
    "full-barrier",
    "getc",
    "gethost",
    "getlogin",
    "hash",
    "head",
    "homedir",
    "import",
    "index",
    "indices",
    "item",
    "kill",
    "kv",
    "lastcall",
    "leave",
    "lf",
    "list",
    "local",
    "lol",
    "made",
    "make",
    "mix",
    "need",
    "nextcallee",
    "nextsame",
    "nextwith",
    "pair",
    "produce",
    "reduce",
    "require",
    "return",
    "return-rw",
    "rindex",
    "samewith",
    "set",
    "shell",
    "signal",
    "skip",
    "sleep",
    "sleep-till",
    "sleep-timer",
    "slip",
    "snip",
    "split",
    "start",
    "syscall",
    "tail",
    "take",
    "tmpdir",
    "use",
];

impl Interpreter {
    /// Check for undeclared type names in EVAL'd code.
    /// Walks the AST looking for BareWord expressions that start with uppercase
    /// and aren't known types, classes, or packages. This mirrors Raku's
    /// compile-time check for undeclared symbols.
    /// A `BEGIN { ... }` block runs at compile time, so it can only see routines
    /// declared *before* it. Calling a sub that is declared *later* in the same
    /// unit (`BEGIN { ohnoes() }; sub ohnoes() {}`) is X::Undeclared::Symbols at
    /// BEGIN time, even though the sub exists by the end of the unit.
    pub(crate) fn check_eval_begin_forward_calls(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // All sub names declared at this level (forward + backward).
        let mut all_subs: HashSet<String> = HashSet::new();
        for s in stmts {
            if let Stmt::SubDecl { name, .. } = s {
                all_subs.insert(name.resolve());
            }
        }
        if all_subs.is_empty() {
            return Ok(());
        }
        let mut declared_before: HashSet<String> = HashSet::new();
        for s in stmts {
            match s {
                Stmt::SubDecl { name, .. } => {
                    declared_before.insert(name.resolve());
                }
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    body,
                } => {
                    let mut calls: HashSet<String> = HashSet::new();
                    Self::collect_call_names_in_stmts(body, &mut calls);
                    // A call to a sub declared only *after* this BEGIN.
                    if let Some(fwd) = calls
                        .iter()
                        .find(|c| all_subs.contains(*c) && !declared_before.contains(*c))
                    {
                        let mut attrs = std::collections::HashMap::new();
                        attrs.insert("symbol".to_string(), Value::str(fwd.clone()));
                        attrs.insert(
                            "message".to_string(),
                            Value::str(format!("Undeclared routine:\n    {} used at line 1", fwd)),
                        );
                        return Err(RuntimeError::typed("X::Undeclared::Symbols", attrs));
                    }
                }
                _ => {}
            }
        }
        Ok(())
    }

    /// Collect the names of bare function calls (`foo(...)`) appearing anywhere
    /// in `stmts`, recursing into nested expressions and block bodies.
    fn collect_call_names_in_stmts(stmts: &[Stmt], out: &mut HashSet<String>) {
        for s in stmts {
            match s {
                Stmt::Expr(e) => Self::collect_call_names_in_expr(e, out),
                Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                    Self::collect_call_names_in_expr(expr, out)
                }
                Stmt::Say(es) | Stmt::Print(es) | Stmt::Put(es) | Stmt::Note(es) => {
                    for e in es {
                        Self::collect_call_names_in_expr(e, out);
                    }
                }
                Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                    Self::collect_call_names_in_stmts(body, out)
                }
                _ => {}
            }
        }
    }

    fn collect_call_names_in_expr(expr: &Expr, out: &mut HashSet<String>) {
        match expr {
            Expr::Call { name, args } => {
                out.insert(name.resolve());
                for a in args {
                    Self::collect_call_names_in_expr(a, out);
                }
            }
            Expr::Binary { left, right, .. } => {
                Self::collect_call_names_in_expr(left, out);
                Self::collect_call_names_in_expr(right, out);
            }
            Expr::Unary { expr, .. } | Expr::Grouped(expr) => {
                Self::collect_call_names_in_expr(expr, out)
            }
            Expr::MethodCall { target, args, .. } => {
                Self::collect_call_names_in_expr(target, out);
                for a in args {
                    Self::collect_call_names_in_expr(a, out);
                }
            }
            Expr::StringInterpolation(parts) => {
                for p in parts {
                    Self::collect_call_names_in_expr(p, out);
                }
            }
            _ => {}
        }
    }

    /// Detect an *illegally post-declared* type: a type name used (as a term or
    /// method invocant) before its textual declaration in the same EVAL'd unit.
    /// Raku resolves type names lexically; using `Foo.bar` and only declaring
    /// `class Foo {}` (or grammar/role/enum/subset) *afterwards* is
    /// X::Undeclared::Symbols with a `post_types` entry, distinct from a
    /// never-declared name (`unk_types`). Mirrors rakudo's CHECK-time check.
    pub(crate) fn check_eval_post_declared_types(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        // Map each top-level type declaration name -> the index of the statement
        // that declares it (first declaration wins).
        let mut decl_index: std::collections::HashMap<String, usize> =
            std::collections::HashMap::new();
        for (i, stmt) in stmts.iter().enumerate() {
            match stmt {
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::SubsetDecl { name, .. }
                | Stmt::EnumDecl { name, .. } => {
                    decl_index.entry(name.resolve()).or_insert(i);
                }
                _ => {}
            }
        }
        if decl_index.is_empty() {
            return Ok(());
        }
        // For each statement, collect the type names it references and flag any
        // whose declaration only appears at a *later* top-level statement.
        for (i, stmt) in stmts.iter().enumerate() {
            let mut refs: Vec<String> = Vec::new();
            Self::collect_type_refs_in_stmt(stmt, &mut refs);
            for name in refs {
                if let Some(&j) = decl_index.get(&name)
                    && j > i
                {
                    let msg = format!("Illegally post-declared type:\n    {} used at line 1", name);
                    return Err(RuntimeError::post_declared_type_symbols(&name, msg));
                }
            }
        }
        Ok(())
    }

    /// Collect uppercase bareword type references (terms and method invocants)
    /// appearing anywhere in `stmt`, recursing into nested bodies.
    fn collect_type_refs_in_stmt(stmt: &Stmt, out: &mut Vec<String>) {
        match stmt {
            Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Fail(e) | Stmt::Goto(e) => {
                Self::collect_type_refs_in_expr(e, out)
            }
            Stmt::Take(e, _) => Self::collect_type_refs_in_expr(e, out),
            Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => {
                Self::collect_type_refs_in_expr(expr, out)
            }
            Stmt::Say(es) | Stmt::Print(es) | Stmt::Put(es) | Stmt::Note(es) => {
                for e in es {
                    Self::collect_type_refs_in_expr(e, out);
                }
            }
            Stmt::Call { args, .. } => {
                for a in args {
                    match a {
                        crate::ast::CallArg::Positional(e)
                        | crate::ast::CallArg::Slip(e)
                        | crate::ast::CallArg::Invocant(e) => {
                            Self::collect_type_refs_in_expr(e, out)
                        }
                        crate::ast::CallArg::Named { value: Some(e), .. } => {
                            Self::collect_type_refs_in_expr(e, out)
                        }
                        crate::ast::CallArg::Named { value: None, .. } => {}
                    }
                }
            }
            Stmt::ClassDecl { body, .. }
            | Stmt::RoleDecl { body, .. }
            | Stmt::SubDecl { body, .. }
            | Stmt::MethodDecl { body, .. }
            | Stmt::Block(body)
            | Stmt::SyntheticBlock(body)
            | Stmt::Default(body)
            | Stmt::Catch(body)
            | Stmt::Control(body) => {
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::Phaser { body, .. } | Stmt::Subtest { body, .. } => {
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::For { iterable, body, .. } => {
                Self::collect_type_refs_in_expr(iterable, out);
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
                ..
            } => {
                Self::collect_type_refs_in_expr(cond, out);
                for s in then_branch {
                    Self::collect_type_refs_in_stmt(s, out);
                }
                for s in else_branch {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::While { cond, body, .. } => {
                Self::collect_type_refs_in_expr(cond, out);
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::Given { topic, body } => {
                Self::collect_type_refs_in_expr(topic, out);
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            Stmt::When { cond, body } => {
                Self::collect_type_refs_in_expr(cond, out);
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            _ => {}
        }
    }

    /// Collect uppercase bareword type references from an expression: standalone
    /// terms (`Foo`) and method invocants (`Foo.bar`), recursing into operands.
    fn collect_type_refs_in_expr(expr: &Expr, out: &mut Vec<String>) {
        match expr {
            Expr::BareWord(name) => {
                if name.chars().next().is_some_and(|c| c.is_ascii_uppercase()) {
                    out.push(name.clone());
                }
            }
            Expr::MethodCall { target, args, .. } => {
                Self::collect_type_refs_in_expr(target, out);
                for a in args {
                    Self::collect_type_refs_in_expr(a, out);
                }
            }
            Expr::Call { args, .. } => {
                for a in args {
                    Self::collect_type_refs_in_expr(a, out);
                }
            }
            Expr::Binary { left, right, .. } => {
                Self::collect_type_refs_in_expr(left, out);
                Self::collect_type_refs_in_expr(right, out);
            }
            Expr::Unary { expr, .. }
            | Expr::PostfixOp { expr, .. }
            | Expr::Grouped(expr)
            | Expr::Itemize(expr)
            | Expr::Eager(expr) => Self::collect_type_refs_in_expr(expr, out),
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                Self::collect_type_refs_in_expr(cond, out);
                Self::collect_type_refs_in_expr(then_expr, out);
                Self::collect_type_refs_in_expr(else_expr, out);
            }
            Expr::AssignExpr { expr, .. } => Self::collect_type_refs_in_expr(expr, out),
            Expr::StringInterpolation(parts)
            | Expr::ArrayLiteral(parts)
            | Expr::CaptureLiteral(parts) => {
                for p in parts {
                    Self::collect_type_refs_in_expr(p, out);
                }
            }
            Expr::Block(body) | Expr::Gather(body) => {
                for s in body {
                    Self::collect_type_refs_in_stmt(s, out);
                }
            }
            _ => {}
        }
    }

    /// Detect a call to an undeclared *routine* in EVAL'd code. Raku resolves
    /// routine names at compile time, so `EVAL '$x = 1; no_such_routine()'`
    /// throws X::Undeclared::Symbols *before* `$x = 1` runs. Only plain bareword
    /// calls (`foo(...)`) are checked — method calls and operator desugarings are
    /// excluded. A name that resolves to a declared sub (here or in the enclosing
    /// pad), a builtin, a proto, or an `&name` callable in scope is fine.
    pub(crate) fn check_eval_undeclared_routines(
        &self,
        stmts: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut declared: HashSet<String> = HashSet::new();
        for s in stmts {
            Self::collect_declared_routine_names(s, &mut declared);
            Self::collect_declared_vars(s, &mut declared);
        }
        let extra: Vec<String> = declared
            .iter()
            .filter_map(|n| n.strip_prefix(['$', '@', '%', '&']).map(str::to_string))
            .collect();
        declared.extend(extra);
        let mut calls: HashSet<String> = HashSet::new();
        Self::collect_call_names_in_stmts(stmts, &mut calls);
        for name in calls {
            // Skip operator desugarings / package-qualified names, and internal
            // synthetic calls the compiler emits (`__MUTSU_SET_META__`, etc.).
            if name.contains(':') || name.starts_with("__") {
                continue;
            }
            if declared.contains(&name)
                || self.has_function(&name)
                || self.has_multi_function(&name)
                || self.has_proto(&name)
                || Self::is_builtin_function(&name)
                || EVAL_KNOWN_ROUTINE_NAMES.contains(&name.as_str())
                || self.env().contains_key(&format!("&{}", name))
                || self.env().contains_key(name.as_str())
                || self.get_our_var(&name).is_some()
            {
                continue;
            }
            let suggestions = self.suggest_type_names(&name);
            return Err(RuntimeError::undeclared_routine_symbols(
                &name,
                format!("Undeclared routine:\n    {} used at line 1", name),
                suggestions,
            ));
        }
        Ok(())
    }

    pub(crate) fn check_eval_undeclared_names(&self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        // Collect class/role names and locally-declared variables/subs from
        // this EVAL code. Both are valid references so they should not be
        // flagged as undeclared bareword names.
        let mut local_classes: HashSet<String> = HashSet::new();
        for stmt in stmts {
            if let Stmt::ClassDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
            if let Stmt::RoleDecl { name, .. } = stmt {
                local_classes.insert(name.resolve());
            }
        }
        let mut declared: HashSet<String> = HashSet::new();
        for stmt in stmts {
            Self::collect_declared_vars(stmt, &mut declared);
        }
        // Normalize collected names: strip leading sigils so bareword lookups
        // (e.g. `foo` after `my &foo := ...`) resolve correctly.
        let bare: Vec<String> = declared
            .iter()
            .filter_map(|n| n.strip_prefix(['$', '@', '%', '&']).map(|s| s.to_string()))
            .collect();
        for n in bare {
            declared.insert(n);
        }
        // Also include any sub/method/grammar/enum names defined at top-level.
        for stmt in stmts {
            Self::collect_declared_routine_names(stmt, &mut declared);
        }
        for stmt in stmts {
            if let Some(name) = self.find_undeclared_name_in_stmt(stmt, &local_classes, &declared) {
                let suggestions = self.suggest_type_names(&name);
                return Err(RuntimeError::undeclared_type_symbols(
                    &name,
                    format!("Undeclared name:\n    {} used at line 1", name),
                    suggestions,
                ));
            }
        }
        Ok(())
    }

    fn collect_declared_routine_names(stmt: &Stmt, out: &mut HashSet<String>) {
        match stmt {
            Stmt::SubDecl { name, .. } | Stmt::MethodDecl { name, .. } => {
                out.insert(name.resolve());
            }
            Stmt::EnumDecl { name, variants, .. } => {
                out.insert(name.resolve());
                for (vname, _) in variants {
                    out.insert(vname.clone());
                }
            }
            Stmt::SyntheticBlock(body) => {
                for s in body {
                    Self::collect_declared_routine_names(s, out);
                }
            }
            _ => {}
        }
    }

    /// Find an undeclared type name (BareWord starting with uppercase) in a statement.
    fn find_undeclared_name_in_stmt(
        &self,
        stmt: &Stmt,
        local_classes: &HashSet<String>,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match stmt {
            Stmt::Expr(expr) => self.find_undeclared_name_in_expr(expr, local_classes, declared),
            // A `constant NAME = <init>` whose initializer references an undeclared
            // bareword (`constant foo = bar`) is X::Undeclared::Symbols. Scoped to
            // `constant` only (custom trait `__constant`) to avoid false positives
            // on ordinary `my $x = ...` initializers that resolve at runtime.
            Stmt::VarDecl {
                expr,
                custom_traits,
                ..
            } if custom_traits.iter().any(|(t, _)| t == "__constant") => {
                self.find_undeclared_name_in_expr(expr, local_classes, declared)
            }
            // `given X { when SomeUndeclaredType {...} }`: a `when` whose condition
            // is an undeclared (bare uppercase) type name is X::Undeclared.
            Stmt::Given { body, .. } => body
                .iter()
                .find_map(|s| self.find_undeclared_name_in_stmt(s, local_classes, declared)),
            Stmt::When { cond, .. } => {
                self.find_undeclared_name_in_expr(cond, local_classes, declared)
            }
            // `enum E (Foo, Bar)` with bare *terms* in the parenthesised body
            // (parsed as a single `__DYNAMIC__` value expression) references
            // undeclared symbols — `<Foo Bar>` is the autoquoting form. Scan the
            // value expressions for undeclared barewords.
            Stmt::EnumDecl { variants, .. } => variants.iter().find_map(|(_, vexpr)| {
                vexpr
                    .as_ref()
                    .and_then(|e| self.find_undeclared_name_in_expr(e, local_classes, declared))
            }),
            // `use Module BareWord` — a bare identifier as a positional import
            // argument is a term reference, not an import symbol (those are
            // strings / `<...>` / `:tags`). An undeclared one is
            // X::Undeclared::Symbols (rakudo: "Undeclared name: ...").
            Stmt::Use { arg: Some(arg), .. } | Stmt::No { arg: Some(arg), .. } => {
                self.find_undeclared_name_in_expr(arg, local_classes, declared)
            }
            _ => None,
        }
    }

    /// Find an undeclared bareword name in an expression.
    /// Checks BareWord nodes that are not known types, classes, functions, or
    /// declared in the current EVAL scope.
    fn find_undeclared_name_in_expr(
        &self,
        expr: &Expr,
        local_classes: &HashSet<String>,
        declared: &HashSet<String>,
    ) -> Option<String> {
        match expr {
            Expr::BareWord(name) => {
                // Skip well-known constants and special names
                if matches!(
                    name.as_str(),
                    "NaN" | "Inf" | "Empty" | "True" | "False" | "Nil" | "Any" | "Mu"
                ) {
                    return None;
                }
                // Skip Raku keywords / special syntactic words that are
                // legitimately parsed as BareWord but should not be treated
                // as undeclared names.
                if matches!(
                    name.as_str(),
                    "self"
                        | "given"
                        | "when"
                        | "default"
                        | "if"
                        | "elsif"
                        | "else"
                        | "unless"
                        | "with"
                        | "without"
                        | "orwith"
                        | "for"
                        | "while"
                        | "until"
                        | "loop"
                        | "repeat"
                        | "do"
                        | "try"
                        | "anon"
                        | "my"
                        | "our"
                        | "has"
                        | "state"
                        | "sub"
                        | "method"
                        | "submethod"
                        | "multi"
                        | "proto"
                        | "only"
                        | "class"
                        | "role"
                        | "grammar"
                        | "token"
                        | "rule"
                        | "regex"
                        | "module"
                        | "package"
                        | "enum"
                        | "subset"
                        | "constant"
                        | "return"
                        | "leave"
                        | "last"
                        | "next"
                        | "redo"
                        | "succeed"
                        | "proceed"
                        | "die"
                        | "fail"
                        | "is"
                        | "does"
                        | "of"
                        | "where"
                        | "but"
                        | "use"
                        | "no"
                        | "need"
                        | "require"
                        | "import"
                        | "lazy"
                        | "eager"
                        | "hyper"
                        | "race"
                        | "sink"
                        | "react"
                        | "supply"
                        | "whenever"
                        | "start"
                        | "gather"
                        | "take"
                        | "quietly"
                        | "now"
                        | "time"
                        | "rand"
                        | "pi"
                        | "e"
                        | "tau"
                        | "i"
                ) {
                    return None;
                }
                // Skip names with :: (package-qualified)
                if name.contains("::") {
                    return None;
                }
                // Skip if it's a known type, class, role, enum, function, or env entry
                if self.has_type(name)
                    || self.has_class(name)
                    || self.has_function(name)
                    || self.has_multi_function(name)
                    || self.env().contains_key(name)
                    // `our`-scoped constants/variables installed in the package
                    // survive in `our_vars` even after their lexical block exits.
                    || self.get_our_var(name).is_some()
                    || local_classes.contains(name.as_str())
                    || declared.contains(name.as_str())
                    || crate::runtime::Interpreter::is_builtin_type(name)
                    || crate::runtime::Interpreter::is_implicit_zero_arg_builtin(name)
                {
                    return None;
                }
                Some(name.clone())
            }
            Expr::Binary { left, right, .. } => self
                .find_undeclared_name_in_expr(left, local_classes, declared)
                .or_else(|| self.find_undeclared_name_in_expr(right, local_classes, declared)),
            Expr::Unary { expr, .. } => {
                self.find_undeclared_name_in_expr(expr, local_classes, declared)
            }
            Expr::StringInterpolation(parts) => {
                for part in parts {
                    if let Some(name) =
                        self.find_undeclared_name_in_expr(part, local_classes, declared)
                    {
                        return Some(name);
                    }
                }
                None
            }
            // A list/array literal (e.g. the parenthesised `(Foo, Bar)` body of an
            // enum) — scan each element for an undeclared bareword term.
            Expr::ArrayLiteral(items) => items
                .iter()
                .find_map(|e| self.find_undeclared_name_in_expr(e, local_classes, declared)),
            _ => None,
        }
    }
}

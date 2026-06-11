use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{AssignOp, CallArg, Expr, PhaserKind, Stmt, make_anon_sub};
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

static STATE_COUNTER: AtomicUsize = AtomicUsize::new(0);
mod expr;
mod expr_binary;
mod expr_block;
mod expr_call;
mod expr_closure;
mod expr_data;
mod expr_helpers;
mod expr_method;
mod expr_ops;
mod expr_postfix;
mod expr_unary;
mod helpers;
mod helpers_ast_utils;
mod helpers_block_inline;
mod helpers_call_args;
mod helpers_control_flow;
mod helpers_do_expr;
mod helpers_dynamic;
mod helpers_ops;
mod helpers_phasers;
mod helpers_stmt_analysis;
mod helpers_sub_body;
mod stmt;

pub(crate) struct Compiler {
    code: CompiledCode,
    local_map: HashMap<String, u32>,
    /// Track type constraints for local variables (for compile-time literal checks).
    local_types: HashMap<String, String>,
    compiled_functions: HashMap<String, CompiledFunction>,
    current_package: String,
    /// True when compiling inside a `unit module`/`unit class`/`unit role`
    /// body. Used to pre-qualify class/role declarations with the package
    /// prefix at compile time, since the runtime does not get a
    /// PackageScope opcode for unit declarations.
    pub(crate) in_unit_package: bool,
    /// The kind of package (`module`/`package`/`grammar`) whose body is
    /// currently being compiled, or `None` in the mainline. Used to raise
    /// X::Attribute::Package when a `has` attribute is declared in a
    /// module/package body (which cannot hold attributes).
    pub(crate) current_package_kind: Option<crate::ast::PackageKind>,
    /// The enclosing package name before closure mangling. Used for `$?PACKAGE`
    /// so that methods inside a class report the class name, not the internal
    /// closure package name.
    pub(crate) enclosing_package: Option<String>,
    tmp_counter: usize,
    dynamic_scope_all: bool,
    dynamic_scope_names: Option<std::collections::HashSet<String>>,
    /// Track dynamic variable accesses (names starting with '*') for postdeclaration check
    accessed_dynamic_vars: std::collections::HashSet<String>,
    /// Whether we are compiling inside a routine (sub/method). `return` outside
    /// a routine must throw X::ControlFlow::Return instead of normal return.
    pub(crate) is_routine: bool,
    /// Whether the enclosing lexical scope contains a routine (sub/method).
    /// Used to decide whether `return` in a non-routine block should perform
    /// a non-local return (via CX::Return) or throw X::ControlFlow::Return.
    pub(crate) lexically_in_routine: bool,
    /// When true, the current VarDecl is from a `:=` bind declaration.
    bind_vardecl: bool,
    /// When true, Index expressions should emit IndexAutovivify instead of
    /// Index.  Set only during scalar `:=` bind VarDecl compilation so that
    /// `my $b := %h<foo><baz>` creates a HashSlotRef.
    scalar_bind_autovivify: bool,
    /// When true (alongside `scalar_bind_autovivify`), the next Index compiled is
    /// the TERMINAL element of the bind RHS (outermost subscript whose value is
    /// bound). A terminal index promotes even a container-valued (Array/Hash) leaf
    /// to a cell. Cleared while compiling the inner `target` so only the outermost
    /// index is terminal.
    bind_terminal: bool,
    /// Variables declared as `constant` (no Scalar container).
    constant_vars: std::collections::HashSet<String>,
    /// Subset of `constant_vars` whose declaring lexical block is still open.
    /// Constants are `our`-scoped (installed in the package), so once their
    /// declaring block has exited, their stale local slot must not be reused —
    /// such bare-word accesses fall back to GetBareWord (package/global lookup).
    constant_vars_in_scope: std::collections::HashSet<String>,
    /// Constants declared in the *current* lexical block only (reset on block
    /// entry). Declaring the same constant twice in one block is an
    /// X::Redeclaration; a shadowing declaration in an inner block is allowed.
    constant_vars_current_scope: std::collections::HashSet<String>,
    /// Local names that are sigilless bindings (declared with `my \Foo = ...`
    /// or as a sigilless parameter).  BareWord resolution only uses GetLocal
    /// for names in this set; `$`-sigiled variables must not shadow type names.
    sigilless_locals: std::collections::HashSet<String>,
    /// Last source line emitted via SetSourceLine (for tracking block definition lines).
    last_source_line: Option<i64>,
    /// Pending writebacks for Index expressions passed to function calls.
    /// After the call returns, if the `is rw` parameter was written to,
    /// we need to write the temp variable value back to the original hash/array slot.
    /// Each entry is (original Index Expr, temp variable name).
    pub(super) pending_index_rw_writebacks: Vec<(Expr, String, String)>,
    /// The current distribution context for $?DISTRIBUTION.
    pub(crate) current_distribution: Option<Value>,
    /// True while compiling a sub-expression whose VALUE is stored/returned/bound
    /// (an *escaping position*): assignment or `:=` RHS, `return`/`fail` operand,
    /// block/routine tail, or a literal element. A closure created while this is
    /// set has its value escape the creating frame, forcing a shared `ContainerRef`
    /// cell for the captured-and-mutated locals it closes over (escape analysis;
    /// see `CompiledCode::closure_escapes`). Default false = the conservative
    /// non-escaping (immediately-invoked) classification, so call arguments and
    /// control-construct blocks never over-box (the #2746 perf guard).
    escaping_position: bool,
    /// True only for the outermost compilation unit (the mainline / a top-level
    /// EVAL). Used to detect placeholder variables (`$^x`, `@_`, ...) that appear
    /// outside any sub or block -> X::Placeholder::Mainline.
    pub(crate) is_mainline: bool,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
            local_map: HashMap::new(),
            local_types: HashMap::new(),
            compiled_functions: HashMap::new(),
            current_package: "GLOBAL".to_string(),
            in_unit_package: false,
            current_package_kind: None,
            enclosing_package: None,
            tmp_counter: 0,
            dynamic_scope_all: false,
            dynamic_scope_names: None,
            accessed_dynamic_vars: std::collections::HashSet::new(),
            is_routine: false,
            lexically_in_routine: false,
            bind_vardecl: false,
            scalar_bind_autovivify: false,
            bind_terminal: false,
            constant_vars: std::collections::HashSet::new(),
            constant_vars_in_scope: std::collections::HashSet::new(),
            constant_vars_current_scope: std::collections::HashSet::new(),
            sigilless_locals: std::collections::HashSet::new(),
            last_source_line: None,
            pending_index_rw_writebacks: Vec::new(),
            current_distribution: None,
            escaping_position: false,
            is_mainline: false,
        }
    }

    /// Run `f` with the escaping-position flag set to `escaping`, restoring the
    /// previous value afterward. Used to mark which syntactic positions cause a
    /// closure created within them to escape its frame (see `escaping_position`).
    pub(super) fn with_escape<R>(&mut self, escaping: bool, f: impl FnOnce(&mut Self) -> R) -> R {
        let saved = self.escaping_position;
        self.escaping_position = escaping;
        let r = f(self);
        self.escaping_position = saved;
        r
    }

    /// Emit a SetSourceLine opcode for the current source line, if known.
    /// Used before method/function calls to ensure ?LINE is up-to-date for
    /// deprecation tracking and error reporting.
    pub(super) fn emit_source_line_if_known(&mut self) {
        if let Some(line) = self.last_source_line {
            self.code.emit(OpCode::SetSourceLine(line));
        }
    }

    pub(crate) fn set_current_package(&mut self, package: String) {
        self.current_package = package;
    }

    pub(crate) fn qualify_package_name(&self, name: &str) -> String {
        if self.current_package == "GLOBAL" || self.current_package.contains("::&") {
            name.to_string()
        } else {
            format!("{}::{}", self.current_package, name)
        }
    }

    fn is_simple_var_expr(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) | Expr::CodeVar(_)
        )
    }

    pub(crate) fn qualify_variable_name(&self, name: &str) -> String {
        if self.current_package.contains("::&") {
            // Sub/method state scopes use package-like names (e.g. GLOBAL::&foo/1)
            // that should not be used to qualify runtime variable access.
            return name.to_string();
        }
        if self.current_package == "GLOBAL" || name.contains("::") {
            return name.to_string();
        }
        if name.is_empty() {
            return name.to_string();
        }
        let first = name.chars().next().unwrap();
        if matches!(first, '_' | '/' | '!' | '?' | '*' | '.' | '=') {
            return name.to_string();
        }
        // Positional capture variables ($0, $1, ...) are never qualified
        if first.is_ascii_digit() && name.chars().all(|c| c.is_ascii_digit()) {
            return name.to_string();
        }
        if let Some(sigil) = name.chars().next()
            && matches!(sigil, '$' | '@' | '%' | '&')
            && name.len() > 1
        {
            return format!("{sigil}{}::{}", self.current_package, &name[1..]);
        }
        format!("{}::{}", self.current_package, name)
    }

    fn alloc_local(&mut self, name: &str) -> u32 {
        if let Some(&slot) = self.local_map.get(name) {
            return slot;
        }
        let slot = self.code.locals.len() as u32;
        let is_simple = name.starts_with('$')
            && !name.starts_with("$*")
            && !name.contains("::")
            && name != "_"
            && !name.starts_with('.')
            && !name.starts_with('!');
        self.code.locals.push(name.to_string());
        self.code.simple_locals.push(is_simple);
        self.local_map.insert(name.to_string(), slot);
        slot
    }

    fn emit_set_named_var(&mut self, name: &str) {
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::SetLocal(slot));
        } else if name.starts_with('!') && name.len() > 1 {
            let slot = self.alloc_local(name);
            self.code.emit(OpCode::SetLocal(slot));
        } else {
            let idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::SetGlobal(idx));
        }
    }

    /// Push the current value of a named scalar variable, mirroring the slot
    /// resolution of `emit_set_named_var` (local slot if present, else global).
    fn emit_get_named_var(&mut self, name: &str) {
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::GetLocal(slot));
        } else {
            let idx = self
                .code
                .add_constant(Value::str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::GetGlobal(idx));
        }
    }

    fn compile_exprs(&mut self, exprs: &[Expr]) {
        for expr in exprs {
            self.compile_expr(expr);
        }
    }

    fn positional_arg_source_name(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::CodeVar(name) => Some(format!("&{}", name)),
            Expr::BareWord(name) => Some(name.to_string()),
            // DoStmt wrapping a VarDecl: `my $c = 42` passed as argument
            Expr::DoStmt(stmt) => Self::extract_varname_from_stmt(stmt),
            // For FatArrow (named args like `:into(%h)`), encode "key=varname"
            // so the VM can write back to the variable after a builtin call.
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
            } => {
                let key = match left.as_ref() {
                    Expr::Literal(Value::Str(s)) => Some(s.as_str()),
                    Expr::BareWord(s) => Some(s.as_str()),
                    _ => None,
                };
                let val_name = Self::extract_inner_varname(right);
                if let (Some(k), Some(v)) = (key, val_name) {
                    Some(format!("{}={}", k, v))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Extract variable name from an expression, including through DoStmt/SyntheticBlock.
    fn extract_inner_varname(expr: &Expr) -> Option<String> {
        match expr {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::CodeVar(name) => Some(format!("&{}", name)),
            Expr::DoStmt(stmt) => Self::extract_varname_from_stmt(stmt),
            _ => None,
        }
    }

    /// Extract variable name from a statement, handling VarDecl and SyntheticBlock.
    fn extract_varname_from_stmt(stmt: &Stmt) -> Option<String> {
        match stmt {
            Stmt::VarDecl { name, .. } | Stmt::Assign { name, .. } => Some(name.clone()),
            Stmt::SyntheticBlock(stmts) => {
                for s in stmts {
                    if let Some(name) = Self::extract_varname_from_stmt(s) {
                        return Some(name);
                    }
                }
                None
            }
            _ => None,
        }
    }

    fn add_arg_sources_constant(&mut self, args: &[Expr]) -> Option<u32> {
        let entries: Vec<Value> = args
            .iter()
            .map(|arg| {
                if let Some(name) = Self::positional_arg_source_name(arg) {
                    Value::str(name)
                } else {
                    Value::Nil
                }
            })
            .collect();
        if entries.iter().all(|v| matches!(v, Value::Nil)) {
            None
        } else {
            Some(self.code.add_constant(Value::array(entries)))
        }
    }

    fn build_for_bind_stmts(
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        param_idx: Option<u32>,
        params: &[String],
    ) -> Vec<Stmt> {
        let bind_stmt = |name: String, expr: Expr| {
            if name.starts_with('&') {
                Stmt::VarDecl {
                    name,
                    expr,
                    type_constraint: None,
                    is_state: false,
                    is_our: false,
                    is_dynamic: false,
                    is_export: false,
                    export_tags: Vec::new(),
                    custom_traits: Vec::new(),
                    where_constraint: None,
                }
            } else {
                Stmt::Assign {
                    name,
                    expr,
                    op: AssignOp::Assign,
                }
            }
        };

        let mut bind_stmts = Vec::new();
        if let Some(single_param) = param
            && param_idx.is_none()
        {
            bind_stmts.push(bind_stmt(single_param.clone(), Expr::Var("_".to_string())));
        }
        if let Some(def) = param_def
            && let Some(sub_params) = &def.sub_signature
        {
            let target_name = param.as_deref().unwrap_or("_").to_string();
            let mut positional_index = 0usize;
            for sub in sub_params {
                if sub.name.is_empty() {
                    continue;
                }
                if sub.named {
                    // Named destructuring `:$key` binds via the accessor method
                    // when the object provides one (Pair.key/.value, object
                    // attribute readers), otherwise by hash key (Hash/Map, which
                    // have no method named after an arbitrary key). Decide at
                    // runtime: `$_.^can("key") ?? $_.key !! $_<key>`.
                    let method_call = Expr::MethodCall {
                        target: Box::new(Expr::Var(target_name.clone())),
                        name: Symbol::intern(&sub.name),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                    };
                    let hash_lookup = Expr::Index {
                        target: Box::new(Expr::Var(target_name.clone())),
                        index: Box::new(Expr::Literal(Value::str(sub.name.clone()))),
                        is_positional: false,
                    };
                    let method_result = Expr::Ternary {
                        cond: Box::new(Expr::MethodCall {
                            target: Box::new(Expr::Var(target_name.clone())),
                            name: Symbol::intern("can"),
                            args: vec![Expr::Literal(Value::str(sub.name.clone()))],
                            modifier: Some('^'),
                            quoted: false,
                        }),
                        then_expr: Box::new(method_call),
                        else_expr: Box::new(hash_lookup),
                    };
                    // If the named param has a sub_signature (e.g. :key($k)),
                    // bind to the sub_signature variable instead of the param name.
                    if let Some(inner_params) = &sub.sub_signature {
                        for inner in inner_params {
                            if !inner.name.is_empty() {
                                bind_stmts
                                    .push(bind_stmt(inner.name.clone(), method_result.clone()));
                            }
                        }
                    } else {
                        bind_stmts.push(bind_stmt(sub.name.clone(), method_result));
                    }
                } else if sub.slurpy && sub.sigilless {
                    // |rest capture parameter: collect remaining elements into a Capture
                    // Generates: rest = \(|target[positional_index..*])
                    let slice_expr = Expr::Index {
                        target: Box::new(Expr::Var(target_name.clone())),
                        index: Box::new(Expr::Binary {
                            left: Box::new(Expr::Literal(Value::Int(positional_index as i64))),
                            op: crate::token_kind::TokenKind::DotDot,
                            right: Box::new(Expr::Whatever),
                        }),
                        is_positional: true,
                    };
                    let capture_expr = Expr::CaptureLiteral(vec![Expr::Unary {
                        op: crate::token_kind::TokenKind::Pipe,
                        expr: Box::new(slice_expr),
                    }]);
                    bind_stmts.push(bind_stmt(sub.name.clone(), capture_expr));
                    // No need to increment positional_index; capture consumes all remaining
                } else {
                    bind_stmts.push(bind_stmt(
                        sub.name.clone(),
                        Expr::Index {
                            target: Box::new(Expr::Var(target_name.clone())),
                            index: Box::new(Expr::Literal(Value::Int(positional_index as i64))),
                            is_positional: false,
                        },
                    ));
                    positional_index += 1;
                }
            }
        }
        // When `$_` is one of the multi-param names (e.g. `-> $_, $name`),
        // binding it first would clobber the source array before other params
        // can read from it.  Defer the `$_` binding to the end.
        let mut deferred_topic = None;
        let mut sigilless_names = Vec::new();
        for (i, p) in params.iter().enumerate() {
            // Sigilless params are prefixed with \\ by the parser.
            let (actual_name, is_sigilless) = if let Some(name) = p.strip_prefix('\\') {
                (name.to_string(), true)
            } else {
                (p.clone(), false)
            };
            let stmt = bind_stmt(
                actual_name.clone(),
                Expr::Index {
                    target: Box::new(Expr::Var("_".to_string())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                    is_positional: false,
                },
            );
            if actual_name == "_" {
                deferred_topic = Some(stmt);
            } else {
                bind_stmts.push(stmt);
            }
            if is_sigilless {
                sigilless_names.push(actual_name);
            }
        }
        if let Some(stmt) = deferred_topic {
            bind_stmts.push(stmt);
        }
        // Mark sigilless params as readonly (e.g. `-> \k, \v`).
        for name in sigilless_names {
            bind_stmts.push(Stmt::MarkSigillessReadonly(name));
        }
        bind_stmts
    }

    fn for_iterable_source_name(iterable: &Expr) -> Option<String> {
        match iterable {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{}", name)),
            Expr::HashVar(name) => Some(format!("%{}", name)),
            Expr::ArrayLiteral(items) if items.len() == 1 => match &items[0] {
                Expr::Var(name) => Some(name.clone()),
                Expr::ArrayVar(name) => Some(format!("@{}", name)),
                Expr::HashVar(name) => Some(format!("%{}", name)),
                _ => None,
            },
            // Handle @a.values, @a.kv, @a.pairs, $pair.value → source is @a / $pair
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty()
                && (*name == "values" || *name == "kv" || *name == "value" || *name == "pairs") =>
            {
                Self::for_iterable_source_name(target)
            }
            // Handle @a.reverse → source is @a (reversed)
            Expr::MethodCall {
                target, name, args, ..
            } if args.is_empty() && *name == "reverse" => Self::for_iterable_source_name(target),
            _ => None,
        }
    }

    /// Check if the for-loop iterable involves a `.reverse` call on a container.
    fn for_iterable_is_reversed(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::MethodCall { name, args, .. }
                if args.is_empty() && *name == "reverse"
        )
    }

    /// Extract per-element variable names when the iterable is a list of
    /// scalar variables (e.g. `($a, $b, $c)`). Returns an empty vec otherwise.
    fn for_iterable_var_names(iterable: &Expr) -> Vec<String> {
        if let Expr::ArrayLiteral(items) = iterable {
            let names: Vec<String> = items
                .iter()
                .filter_map(|item| match item {
                    Expr::Var(name) => Some(name.clone()),
                    _ => None,
                })
                .collect();
            if names.len() == items.len() {
                return names;
            }
        }
        Vec::new()
    }

    /// Detect if the iterable is a `.kv` method call (key-value pairs).
    fn for_iterable_is_kv(iterable: &Expr) -> bool {
        matches!(
            iterable,
            Expr::MethodCall { name, args, .. }
                if args.is_empty() && *name == "kv"
        )
    }

    fn normalize_for_iterable(&self, iterable: &Expr) -> Expr {
        match iterable {
            // Scalar variables are item containers in `for` and should not be flattened.
            // Exception: `constant $x` binds without a Scalar container, so `for $x`
            // should iterate the elements (like sigilless variables).
            Expr::Var(name) if !self.constant_vars.contains(name) => {
                Expr::ArrayLiteral(vec![iterable.clone()])
            }
            _ => iterable.clone(),
        }
    }

    pub(crate) fn compile(
        mut self,
        stmts: &[Stmt],
    ) -> (CompiledCode, HashMap<String, CompiledFunction>) {
        // Hoist top-level `use Test` declarations to the front (Raku `use` is
        // BEGIN-time, so test functions are available throughout the file even
        // when `plan`/`ok` appear textually before `use Test;`).
        let test_hoisted;
        let stmts = if let Some(r) = Self::hoist_test_use_decls(stmts) {
            test_hoisted = r;
            &test_hoisted[..]
        } else {
            stmts
        };
        // Reorder stub class declarations so real definitions come right
        // after stubs (Raku class declarations are compile-time).
        let reordered;
        let stmts = if let Some(r) = Self::reorder_stub_class_decls(stmts) {
            reordered = r;
            &reordered[..]
        } else {
            stmts
        };
        // A placeholder variable ($^x, @_, ...) directly in the mainline is
        // outside any sub or block -> X::Placeholder::Mainline. Emit the Die
        // first so it fires before any other statement runs.
        if self.is_mainline
            && let Some(ph) = crate::ast::collect_unattached_placeholders(stmts)
                .into_iter()
                .next()
        {
            let err = Self::placeholder_scope_error("mainline", &ph);
            let idx = self.code.add_constant(err);
            self.code.emit(OpCode::LoadConst(idx));
            self.code.emit(OpCode::Die);
            self.code.compute_needs_env_sync();
            return (self.code, self.compiled_functions);
        }
        self.hoist_sub_decls(stmts, false);
        // If the top-level body contains a CATCH or CONTROL block, wrap in
        // an implicit try so the phaser can observe exceptions / control
        // signals from the surrounding statements.
        let has_catch = stmts
            .iter()
            .any(|s| matches!(s, Stmt::Catch(_) | Stmt::Control(_)));
        if has_catch {
            self.compile_try(stmts, &None);
            self.code.emit(OpCode::Pop);
        } else if self.is_routine && Self::has_block_enter_leave_phasers(stmts) {
            self.compile_toplevel_block_scope(stmts);
        } else {
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                if is_last {
                    match stmt {
                        Stmt::Expr(expr) => {
                            // Tail expression becomes the body value -> escapes.
                            self.with_escape(true, |c| c.compile_expr(expr));
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::Call { name, args } => {
                            let positional: Option<Vec<Expr>> = args
                                .iter()
                                .map(|arg| match arg {
                                    crate::ast::CallArg::Positional(expr) => Some(expr.clone()),
                                    _ => None,
                                })
                                .collect();
                            if let Some(positional_args) = positional {
                                self.compile_expr(&Expr::Call {
                                    name: *name,
                                    args: positional_args,
                                });
                                self.code.emit(OpCode::SetTopic);
                                continue;
                            }
                        }
                        Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                            if Self::has_block_placeholders(body) {
                                self.compile_stmt(&Stmt::Die(Expr::Literal(Value::str(
                                    "Implicit placeholder parameters are not available in bare nested blocks"
                                        .to_string(),
                                ))));
                                continue;
                            }
                            self.compile_block_inline(body);
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::If {
                            cond,
                            then_branch,
                            else_branch,
                            binding_var,
                        } if binding_var.is_none() => {
                            self.compile_if_value(cond, then_branch, else_branch);
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::VarDecl { name, .. } => {
                            // VarDecl as last statement: compile normally, then
                            // load the declared variable back and set as topic
                            // so that implicit return works correctly.
                            let var_name = name.clone();
                            self.compile_stmt(stmt);
                            let slot = self.alloc_local(&var_name);
                            self.code.emit(OpCode::GetLocal(slot));
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        _ => {}
                    }
                }
                self.compile_stmt(stmt);
            }
        }
        self.code.compute_needs_env_sync();
        (self.code, self.compiled_functions)
    }

    fn compile_toplevel_block_scope(&mut self, stmts: &[Stmt]) {
        let idx = self.code.emit(OpCode::BlockScope {
            pre_end: 0,
            enter_end: 0,
            body_end: 0,
            keep_start: 0,
            undo_start: 0,
            post_start: 0,
            end: 0,
        });
        Self::compile_pre_phasers(self, stmts);
        self.code.patch_block_pre_end(idx);
        for s in stmts {
            if let Stmt::Phaser {
                kind: PhaserKind::Enter,
                body,
            } = s
            {
                for inner in body {
                    self.compile_stmt(inner);
                }
            }
        }
        self.code.patch_block_enter_end(idx);
        let body_stmts: Vec<&Stmt> = stmts
            .iter()
            .filter(|s| {
                !matches!(
                    s,
                    Stmt::Phaser {
                        kind: PhaserKind::Enter
                            | PhaserKind::Leave
                            | PhaserKind::Keep
                            | PhaserKind::Undo
                            | PhaserKind::Pre
                            | PhaserKind::Post,
                        ..
                    }
                )
            })
            .collect();
        for (i, s) in body_stmts.iter().enumerate() {
            let is_last = i == body_stmts.len() - 1;
            if is_last {
                self.compile_last_stmt_as_topic(s);
            } else {
                self.compile_stmt(s);
            }
        }
        self.code.patch_block_body_end(idx);
        self.code.patch_block_keep_start(idx);
        {
            let mut prev_guard: Option<usize> = None;
            for s in stmts.iter().rev() {
                if let Stmt::Phaser { kind, body } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Keep)
                {
                    if let Some(pg) = prev_guard {
                        self.code.patch_leave_guard_next(pg);
                    }
                    let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                    for inner in body {
                        self.compile_stmt(inner);
                    }
                    prev_guard = Some(guard_idx);
                }
            }
            if let Some(pg) = prev_guard {
                self.code.patch_leave_guard_next(pg);
            }
        }
        self.code.patch_block_undo_start(idx);
        {
            let mut prev_guard: Option<usize> = None;
            for s in stmts.iter().rev() {
                if let Stmt::Phaser { kind, body } = s
                    && matches!(kind, PhaserKind::Leave | PhaserKind::Undo)
                {
                    if let Some(pg) = prev_guard {
                        self.code.patch_leave_guard_next(pg);
                    }
                    let guard_idx = self.code.emit(OpCode::LeaveGuard { next: 0 });
                    for inner in body {
                        self.compile_stmt(inner);
                    }
                    prev_guard = Some(guard_idx);
                }
            }
            if let Some(pg) = prev_guard {
                self.code.patch_leave_guard_next(pg);
            }
        }
        self.code.patch_block_post_start(idx);
        Self::compile_post_phasers(self, stmts);
        self.code.patch_loop_end(idx);
    }
}

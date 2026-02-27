use std::collections::HashMap;
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::ast::{AssignOp, CallArg, Expr, PhaserKind, Stmt, make_anon_sub};
use crate::opcode::{CompiledCode, CompiledFunction, OpCode};
use crate::token_kind::TokenKind;
use crate::value::Value;

static STATE_COUNTER: AtomicUsize = AtomicUsize::new(0);
mod expr;
mod helpers;
mod stmt;

pub(crate) struct Compiler {
    code: CompiledCode,
    local_map: HashMap<String, u32>,
    compiled_functions: HashMap<String, CompiledFunction>,
    current_package: String,
    tmp_counter: usize,
    dynamic_scope_all: bool,
    dynamic_scope_names: Option<std::collections::HashSet<String>>,
    /// Track dynamic variable accesses (names starting with '*') for postdeclaration check
    accessed_dynamic_vars: std::collections::HashSet<String>,
}

impl Compiler {
    pub(crate) fn new() -> Self {
        Self {
            code: CompiledCode::new(),
            local_map: HashMap::new(),
            compiled_functions: HashMap::new(),
            current_package: "GLOBAL".to_string(),
            tmp_counter: 0,
            dynamic_scope_all: false,
            dynamic_scope_names: None,
            accessed_dynamic_vars: std::collections::HashSet::new(),
        }
    }

    pub(crate) fn set_current_package(&mut self, package: String) {
        self.current_package = package;
    }

    pub(crate) fn qualify_package_name(&self, name: &str) -> String {
        if self.current_package == "GLOBAL" || name.contains("::") {
            name.to_string()
        } else {
            format!("{}::{}", self.current_package, name)
        }
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
        self.code.locals.push(name.to_string());
        self.local_map.insert(name.to_string(), slot);
        slot
    }

    fn emit_set_named_var(&mut self, name: &str) {
        if let Some(&slot) = self.local_map.get(name) {
            self.code.emit(OpCode::SetLocal(slot));
        } else {
            let idx = self
                .code
                .add_constant(Value::Str(self.qualify_variable_name(name)));
            self.code.emit(OpCode::SetGlobal(idx));
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
            _ => None,
        }
    }

    fn add_arg_sources_constant(&mut self, args: &[Expr]) -> Option<u32> {
        let entries: Vec<Value> = args
            .iter()
            .map(|arg| {
                if let Some(name) = Self::positional_arg_source_name(arg) {
                    Value::Str(name)
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
                    let method_result = Expr::MethodCall {
                        target: Box::new(Expr::Var(target_name.clone())),
                        name: sub.name.clone(),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
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
                } else {
                    bind_stmts.push(bind_stmt(
                        sub.name.clone(),
                        Expr::Index {
                            target: Box::new(Expr::Var(target_name.clone())),
                            index: Box::new(Expr::Literal(Value::Int(positional_index as i64))),
                        },
                    ));
                    positional_index += 1;
                }
            }
        }
        for (i, p) in params.iter().enumerate() {
            bind_stmts.push(bind_stmt(
                p.clone(),
                Expr::Index {
                    target: Box::new(Expr::Var("_".to_string())),
                    index: Box::new(Expr::Literal(Value::Int(i as i64))),
                },
            ));
        }
        bind_stmts
    }

    fn for_iterable_source_name(iterable: &Expr) -> Option<String> {
        match iterable {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayLiteral(items) if items.len() == 1 => match &items[0] {
                Expr::Var(name) => Some(name.clone()),
                _ => None,
            },
            _ => None,
        }
    }

    fn normalize_for_iterable(iterable: &Expr) -> Expr {
        match iterable {
            // Scalar variables are item containers in `for` and should not be flattened.
            Expr::Var(_) => Expr::ArrayLiteral(vec![iterable.clone()]),
            _ => iterable.clone(),
        }
    }

    fn for_rw_writeback_stmt(
        param: &Option<String>,
        param_def: &Option<crate::ast::ParamDef>,
        iterable: &Expr,
    ) -> Option<Stmt> {
        let has_rw = param_def
            .as_ref()
            .is_some_and(|def| def.traits.iter().any(|t| t == "rw"));
        if !has_rw {
            return None;
        }
        let param_name = param.as_ref()?;
        let source_name = Self::for_iterable_source_name(iterable)?;
        Some(Stmt::Assign {
            name: source_name,
            expr: Expr::Var(param_name.clone()),
            op: AssignOp::Assign,
        })
    }

    pub(crate) fn compile(
        mut self,
        stmts: &[Stmt],
    ) -> (CompiledCode, HashMap<String, CompiledFunction>) {
        self.hoist_sub_decls(stmts);
        // If the top-level body contains a CATCH block, wrap in implicit try.
        let has_catch = stmts.iter().any(|s| matches!(s, Stmt::Catch(_)));
        if has_catch {
            self.compile_try(stmts, &None);
            self.code.emit(OpCode::Pop);
        } else {
            for (i, stmt) in stmts.iter().enumerate() {
                let is_last = i == stmts.len() - 1;
                if is_last {
                    match stmt {
                        Stmt::Expr(expr) => {
                            self.compile_expr(expr);
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        Stmt::Block(body) | Stmt::SyntheticBlock(body) => {
                            if Self::has_block_placeholders(body) {
                                self.compile_stmt(&Stmt::Die(Expr::Literal(Value::Str(
                                    "Implicit placeholder parameters are not available in bare nested blocks"
                                        .to_string(),
                                ))));
                                continue;
                            }
                            self.compile_block_inline(body);
                            self.code.emit(OpCode::SetTopic);
                            continue;
                        }
                        _ => {}
                    }
                }
                self.compile_stmt(stmt);
            }
        }
        (self.code, self.compiled_functions)
    }
}
